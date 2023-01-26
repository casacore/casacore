//# FitGaussian.cc: Multidimensional fitter class for Gaussians
//# Copyright (C) 2001,2002
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

#ifndef SCIMATH_FITGAUSSIAN_TCC
#define SCIMATH_FITGAUSSIAN_TCC

#include <casacore/scimath/Fitting/FitGaussian.h>

#include <casacore/scimath/Fitting/NonLinearFitLM.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/scimath/Functionals/Gaussian3D.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// The parameter chisqcriteria has been replaced with maximumRMS, which is
// the square root of the average error-squared per pixel.

// The fitter no longer returns the last (failed) fit if nothing meets the RMS
// criteria.  Instead it returns the fit which yielded the lowest chisquared.

// IMPR: The retry system is unwieldy and very slow.  It would be much
// better generate retry paramters entirely automatically, requiring only
// the number of tries to attempt from the user.  This could be pretty easy
// or quite complicated depending on how sophisticated a retry system is
// desired:
//  Simple: control size of retry matrix and skip all tries above ntries
//  Complicated: actually use properties of failed attempts to move the start
//    point in an intelligent manner (ie orthogonal to start-end axis)



template <class T>
FitGaussian<T>::FitGaussian()
{
  itsDimension = 0;
  itsNGaussians = 0;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsSuccess = 0;
}

template <class T>
FitGaussian<T>::FitGaussian(uint32_t dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uint32_t dimensions) - "
                    "dimensions must be 1, 2, or 3"));

  itsDimension = dimensions;
  itsNGaussians = 0;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsSuccess = 0;
}

template <class T>
FitGaussian<T>::FitGaussian(uint32_t dimensions, uint32_t numgaussians)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uint32_t dimensions, "
                    "uInt numgaussians) - dimensions must be 1, 2, or 3"));
 
  itsDimension = dimensions;
  itsNGaussians = numgaussians;
  itsMask.resize(itsNGaussians, itsDimension*3);
  itsMask = 1;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsSuccess = 0;
}

template <class T>
void FitGaussian<T>::setDimensions(uint32_t dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::setDimenions(uint32_t dimensions)"
                    " - dimensions must be 1, 2, or 3")); 
  itsDimension = dimensions;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsRetryFctr.resize();
  itsFirstEstimate.resize();
  itsMask.resize();
  if (itsNGaussians) {
    itsMask.resize(itsNGaussians, itsDimension*3); itsMask = 1;
  }
}

template <class T>
void FitGaussian<T>::setNumGaussians(uint32_t numgaussians)
{
  itsNGaussians = numgaussians;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsRetryFctr.resize();
  itsFirstEstimate.resize();
  itsMask.resize();
  if (itsDimension*3 != 0 && itsNGaussians) {
    itsMask.resize(itsNGaussians, itsDimension*3); itsMask = 1;
  }
}

template <class T>
void FitGaussian<T>::setFirstEstimate(const Matrix<T>& estimate)
{
  if ((estimate.nrow() != itsNGaussians) || 
      (estimate.ncolumn() != itsDimension*3))
    throw(AipsError("FitGaussian<T>::setfirstestimate(const Matrix<T>& "
                    "estimate) - estimate must be of shape "
                    "[(ngaussians) , (dimension x 3)]"));

  itsFirstEstimate.resize();
  itsFirstEstimate = estimate;
}

template <class T>
void FitGaussian<T>::setRetryFactors()
{
  setRetryFactors(defaultRetryMatrix());
}

template <class T>
void FitGaussian<T>::setRetryFactors(const Matrix<T>& retryfactors)
{

  if (retryfactors.ncolumn() != itsDimension*3)
    throw(AipsError("FitGaussian<T>::setretryfactors(const Matrix<T>&"
                    " retryfactors) - retryfactors must have numcolumns = "
                    " dimension x 3")); 
  itsRetryFctr.resize(); 
  itsRetryFctr = retryfactors;
}


template <class T>
bool &FitGaussian<T>::mask(uint32_t gaussian, uint32_t parameter)
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsDimension*3))
    throw(AipsError("FitGaussian<T>::mask(uint32_t gaussian, uint32_t parameter)"
                    " - index out of range"));
  return itsMask(gaussian, parameter);
}

template <class T>
const bool &FitGaussian<T>::mask(uint32_t gaussian, uint32_t parameter) const
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsDimension*3))
    throw(AipsError("FitGaussian<T>::mask(uint32_t gaussian, uint32_t parameter"
                    " const - index out of range"));
  return itsMask(gaussian, parameter);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                               T maximumRMS, uint32_t maxiter, 
                               T convcriteria)
{
  //Same as below, with all sigma = 1.

  Vector<T> sigma(f.nelements(), 1);

  return fit(pos, f, sigma, maximumRMS, maxiter, convcriteria);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                              const Vector<T>& sigma, T maximumRMS,
                              uint32_t maxiter, T convcriteria)
{
  //Perform the fitting to the data.  Sets up NonLinearFitLM with the specified
  //number of gaussians and starts fitting.  If the fit fails or converges
  //with an RMS above maximumRMS, it retries by multiplying certain
  //estimate gaussians by the retry matrix.

  uint32_t const ngpars = itsDimension*3;

  if (pos.ncolumn() != itsDimension) 
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uint32_t maxiter, T convcriteria) - "
                    " pos is of wrong number of dimensions."));

  if ((pos.nrow() != f.nelements()) || (pos.nrow() != sigma.nelements()))
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uint32_t maxiter, T convcriteria) - "
                    " pos, f, and sigma must all have same length."));

  if (pos.nrow() <= 0)
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uint32_t maxiter, T convcriteria) - "
                    " pos contains no data."));



  NonLinearFitLM<T> fitter(0);
  Vector<T> solution,errors;
  Matrix<T> startparameters(itsNGaussians, ngpars);
  itsSolutionParameters.resize(itsNGaussians, ngpars);
  itsSolutionErrors.resize(itsNGaussians,ngpars);

 Block<Gaussian1D<AutoDiff<T> > > gausscomp1d((itsDimension==1)*itsNGaussians);
 Block<Gaussian2D<AutoDiff<T> > > gausscomp2d((itsDimension==2)*itsNGaussians);
 Block<Gaussian3D<AutoDiff<T> > > gausscomp3d((itsDimension==3)*itsNGaussians);

  fitter.setMaxIter(maxiter);
  fitter.setCriteria(convcriteria);
 
  Vector<int32_t> targetmask(itsNGaussians,-1); //should rename this... 
  uint32_t attempt = 0;                         //overall attempt number
  int32_t fitfailure;
  T bestRMS = C::flt_max;  //how to template this properly...
  
  itsSuccess = 0; 

  if (itsMaxRetries > 0 && nRetryFactors() == 0) {
    setRetryFactors();
  }

  //If there are not enough data points, fix some parameters to the estimate

  if (itsNGaussians >= pos.nrow())
  {
    for (uint32_t p = 1; p < ngpars; p++) {
      for (uint32_t g = 0; g < itsNGaussians; g++) {
        mask(g,p) = 0;
      }
    }
    if (itsNGaussians > pos.nrow()) {
      uint32_t g = 0;
      while (countFreeParameters() > pos.nrow()) {
        mask(g,0) = 0;
        g++;
      }
    }
  }

  uint32_t fixpar = ngpars;
  while (countFreeParameters() > pos.nrow()) {
    fixpar--;
    if (fixpar == itsDimension * 2) fixpar = itsDimension; //fix widths last
    if (fixpar == 0) fixpar = itsDimension * 2; 
    for (uint32_t g = 0; g < itsNGaussians; g++) {
      mask(g,fixpar) = 1;
    }
  }


  //Begin fitting
  
  Timer timer;
  timer.mark();

  do {   

    // Modify the estimate according to the retry factors, if necessary.

    if ((attempt) && (attempt <= itsMaxRetries)) {
      if (pow(int32_t(nRetryFactors()),int32_t(itsNGaussians)) <int32_t(itsMaxRetries)*3/2)
      {
        //Eventual redundancy is very likely, so make the retry matrix bigger.
        expandRetryMatrix(1);
      }

      Time tmptime(1982,8,31,10);
      MLCG gen(int32_t(tmptime.age()));
      //DiscreteUniform retgen(&gen, -nRetryFactors(), nRetryFactors()-1);
      // any negative number means use the unaltered estimate (50% chance)
   
      //The new (2002/07/11) retry system is very simple: the retry targets
      //are chosen at random, as is the selection from the retry matrix.

      uint32_t ntargets = (gen.asuInt() % (1 + itsNGaussians / 2)) + 1;

      targetmask = -1;
      for (uint32_t i = 0; i < ntargets; i++) {
        uint32_t t = gen.asuInt() % itsNGaussians;
        targetmask(t) = int32_t(gen.asuInt() % nRetryFactors());
      }     
    }

    //cout << targetmask << endl;

    // Set the initial estimate and create the component gaussian functionals
    // used in fitting.
    
    for (uint32_t g = 0; g < itsNGaussians; g++) {
      for (uint32_t p = 0; p < ngpars; p++) {
        startparameters(g,p) = itsFirstEstimate(g,p);
        if (targetmask(g) >= 0) {
          //apply retry factors
          int32_t retry = targetmask(g);
          if (itsDimension == 1) {
            if (p == 1) startparameters(g,p) += itsRetryFctr(retry,p);
            else        startparameters(g,p) *= itsRetryFctr(retry,p);
	  }
          if (itsDimension == 2) {
	    if ((p == 1) || (p == 2) || (p == 5)) {
              startparameters(g,p) += itsRetryFctr(retry,p);
            } else {
              startparameters(g,p) *= itsRetryFctr(retry,p);
            }
	  }
          if (itsDimension == 3) {
	    if ((p == 1) || (p == 2) || (p == 3) || (p == 7) || (p == 8)) {
              startparameters(g,p) += itsRetryFctr(retry,p);
            } else {
              startparameters(g,p) *= itsRetryFctr(retry,p);
            }
	  }
	}
        if (itsDimension==1) { 
          gausscomp1d[g][p]=AutoDiff<T>(startparameters(g,p), ngpars, p);
          gausscomp1d[g].mask(p) = itsMask(g,p);
	}
        if (itsDimension==2) {
          gausscomp2d[g][p]=AutoDiff<T>(startparameters(g,p), ngpars, p);
          gausscomp2d[g].mask(p) = itsMask(g,p);
	}
        if (itsDimension==3) {
          gausscomp3d[g][p]=AutoDiff<T>(startparameters(g,p), ngpars, p);
          gausscomp3d[g].mask(p) = itsMask(g,p);
	}
      }
    }

    // Create the fitting function by summing up the component gaussians.
   
    CompoundFunction<AutoDiff<T> > sumfunc;
    for (uint32_t g = 0; g < itsNGaussians; g++) {
      if (itsDimension==1) sumfunc.addFunction(gausscomp1d[g]);
      if (itsDimension==2) sumfunc.addFunction(gausscomp2d[g]);
      if (itsDimension==3) sumfunc.addFunction(gausscomp3d[g]);
    }

    fitter.setFunction(sumfunc);  //sumgauss
    fitter.setCriteria(convcriteria);

    solution.resize(0);
    errors.resize(0);
    fitfailure = 0;
    attempt++;

    LogIO os(LogOrigin("FitGaussian", "fit", WHERE));
    os << LogIO::DEBUG1 << "Attempt " << attempt << ": ";
  
    // Perform the fit, and check for problems with the results.
    
    try {
       solution = fitter.fit(pos, f, sigma);
       errors = fitter.errors();
    } catch (AipsError& fittererror) {
      string errormessage;
      errormessage = fittererror.getMesg();
      os << LogIO::DEBUG1 << "Unsuccessful - Error during fitting." << LogIO::POST;
      os  << LogIO::DEBUG1 << errormessage << LogIO::POST;
      fitfailure = 2;
    } 
    if (!fitter.converged() && !fitfailure) {
      fitfailure = 1;
      os <<LogIO::DEBUG1 << "Unsuccessful - Failed to converge." << LogIO::POST;
    }
    if (fitter.converged()) {
      itsChisquare = fitter.chiSquare();
      if (itsChisquare < 0) {
        os<<   LogIO::DEBUG1 << "Unsuccessful - ChiSquare of "<< itsChisquare << "is negative."
             << LogIO::POST;
        fitfailure = 3;
      }
      else if (isNaN(itsChisquare)){
        os <<  LogIO::DEBUG1 << "Unsuccessful - Convergence to NaN result" << LogIO::POST;
        fitfailure = 3;
      }
      else {

        for (uint32_t g = 0; g < itsNGaussians; g++) {    
          if ((itsDimension == 1  &&  solution(g*ngpars+2) < 0)   ||
	      (itsDimension == 2  && (solution(g*ngpars+3) < 0  || 
				      solution(g*ngpars+4) < 0))  ||
              (itsDimension == 3  && (solution(g*ngpars+4) < 0  || 
	                              solution(g*ngpars+5) < 0  ||
                                      solution(g*ngpars+6) < 0))) { 
            fitfailure = 4;
            os <<  LogIO::DEBUG1 << "Unsuccessful - Negative axis widths not permissible." << LogIO::POST;
	    break;
	  }
	}

        if (!fitfailure) {
          itsRMS = sqrt(itsChisquare / f.nelements());
          if (itsRMS > maximumRMS) {
            os <<  LogIO::DEBUG1 << "Unsuccessful - RMS of " << itsRMS;
            os <<  LogIO::DEBUG1 << " is outside acceptible limits." << LogIO::POST;
            fitfailure = 5;
          }
          else
	  {
           os <<  LogIO::DEBUG1  << "Converged after " << fitter.currentIteration() 
                 << " iterations" << LogIO::POST;
	  }

	  if (itsRMS < bestRMS) { 
            //best fit so far - write parameters to solution matrix
            for (uint32_t g = 0; g < itsNGaussians; g++) {  
              for (uint32_t p = 0; p < ngpars; p++) {
                itsSolutionParameters(g,p) = solution(g*ngpars+p);
                itsSolutionErrors(g,p) = errors(g*ngpars+p);
              }
            }
            bestRMS = itsRMS;
            itsSuccess = 1;   //even if it's not a complete success
	  }

	}

      }
    }     
  } while ((fitfailure) && (attempt <= itsMaxRetries) && 
                           (timer.real() < itsMaxTime));


// If at least one convergent solution has been found, return its parameters

  if (itsSuccess) {
    if (fitfailure) {
      if (attempt > itsMaxRetries) {
        os <<  LogIO::DEBUG1 << "Retry limit reached, ";
      }
      else if (timer.real() >= itsMaxTime) { 
        os <<  LogIO::DEBUG1 << "Time limit reached, ";
      }
      os <<  LogIO::DEBUG1 << "no fit satisfies RMS criterion; using best available fit"<< LogIO::POST;
    }
    correctParameters(itsSolutionParameters);
    return itsSolutionParameters;
  }

// Otherwise, return all zeros 
 
  os<< LogIO::WARN << "FAILURE - could not find acceptible convergent solution." << endl;
  itsSuccess = 0;

  for (uint32_t g = 0; g < itsNGaussians; g++)  {   
    for (uint32_t p = 0; p < ngpars; p++) {
      itsSolutionParameters(g,p) = T(0.0);
      itsSolutionErrors(g,p) = T(0.0);
    }
  }
//
  return itsSolutionParameters;
   
}

template <class T>
void FitGaussian<T>::correctParameters(Matrix<T>& parameters)
{
  //bring rotation/axis values into the stated domain.

  for (uint32_t g = 0; g < itsNGaussians; g++) {     
    if (itsDimension == 2) {
      if (parameters(g,4) > 1) {
        parameters(g,3) *= parameters(g,4);
        parameters(g,4) = 1/parameters(g,4);      //swap axes
        parameters(g,5) += C::pi_2;
      }
      if (abs(parameters(g,5)) > 1e+5) continue;  //spin control

      //IMPR: a useful thing to do would be to retry the fit with all other
      //params fixed if the PA ends up crazy like this.
 
      while (parameters(g,5) < 0)  parameters(g,5) += C::pi;
      while (parameters(g,5) > C::pi) parameters(g,5) -= C::pi;
    }
    if (itsDimension == 3) {  
      if (abs(parameters(g,7)) > 1e+5) continue;  //spin control 
      while (parameters(g,7) < -C::pi_2) parameters(g,7) += C::pi;
      while (parameters(g,7) > C::pi_2) parameters(g,7) -= C::pi;

      if (abs(parameters(g,8)) > 1e+5) continue;  //spin control 
      while (parameters(g,8) < -C::pi_2) parameters(g,8) += C::pi;
      while (parameters(g,8) > C::pi_2) parameters(g,8) -= C::pi;

      if (abs(parameters(g,7)) > C::pi_4) {
        //swap y/x axes
        T temp = parameters(g,4);
        parameters(g,4) = parameters(g,5);
        parameters(g,5) = temp;
        if (parameters(g,7) > 0)
          parameters(g,7) -= C::pi_2;
        else
          parameters(g,7) += C::pi_2;
      }
      if (abs(parameters(g,8)) > C::pi_4) {
        //swap z/x axes
        T temp = parameters(g,4);
        parameters(g,4) = parameters(g,6);
        parameters(g,6) = temp;
        if (parameters(g,8) > 0) {
          parameters(g,8) -= C::pi_2;
        } else {
          parameters(g,8) += C::pi_2;
        }
      }
    }
  }
  return;
}

template <class T>
bool FitGaussian<T>::converged()
{
  //Did the fitter converge to an acceptible value?
  return itsSuccess;
}

template <class T>
T FitGaussian<T>::chisquared()
{
  //Chisquared of completed fit   IMPR: shouldn't work if no convergence?
  return itsChisquare;
}

template <class T>
T FitGaussian<T>::RMS()
{
  //RMS of completed fit
  return itsRMS;
}



template <class T>
Matrix<T> FitGaussian<T>::defaultRetryMatrix()
{
  Matrix<T> rt(7,itsDimension * 3);
  rt.column(0) = 1;  
  if (itsDimension == 1) {
    rt.column(1) = 0;
    rt(0,2) = 0.5;
    rt(1,2) = 0.6;
    rt(2,2) = 0.7;
    rt(3,2) = 0.8;
    rt(4,2) = 0.9;
    rt(5,2) = 1.3;
    rt(6,2) = 2.0;
  } 
  if (itsDimension == 2) {
    rt.column(1) = 0;
    rt.column(2) = 0;
    rt(0,3) = 1;    rt(0,4) = 0.6;  rt(0,5) = 0;
    rt(1,3) = 0.5;  rt(1,4) = 1;    rt(1,5) = 0;
    rt(2,3) = 1;    rt(2,4) = 1;    rt(2,5) = 0.52;
    rt(3,3) = 1;    rt(3,4) = 1;    rt(3,5) = -0.52;
    rt(4,3) = 1.5;  rt(4,4) = 1;    rt(4,5) = 0;
    rt(5,3) = 1;    rt(5,4) = 0.6;  rt(5,5) = 0.52;
    rt(6,4) = 1;    rt(6,4) = 0.6;  rt(6,5) = -0.52;
  }
  if (itsDimension == 3) {
    rt.column(1) = 0;
    rt.column(2) = 0;
    rt.column(3) = 0;
    rt(0,4) = 1.5; rt(0,5) = 0.9; rt(0,6) = 0.5; rt(0,7) = 0;   rt(0,8) = 0;
    rt(1,4) = 0.4; rt(1,5) = 0.4; rt(1,6) = 0.4; rt(1,7) = 0;   rt(1,8) = 0;
    rt(2,4) = 1.5; rt(2,5) = 1.5; rt(2,6) = 1;   rt(2,7) = 0.5; rt(2,8) = 0;
    rt(3,4) = 1.2; rt(3,5) = 1.2; rt(3,6) = 1.5; rt(3,7) = 0;   rt(3,8) = 0.5;
    rt(4,4) = 1.5; rt(4,5) = 1.5; rt(4,6) = 1;   rt(4,7) =-0.5; rt(4,8) = 0;
    rt(5,4) = 1.5; rt(5,5) = 1.5; rt(5,6) = 1.5; rt(5,7) = 0.5; rt(5,8) = 0.5;
    rt(6,4) = 1.5; rt(6,5) = 1.5; rt(6,6) = 1.5; rt(6,7) =-0.5; rt(6,8) =-0.5;
    //increasing axis sizes on rotation only useful if estimated rotation is 0
  }
  return rt;
}

template <class T>
void FitGaussian<T>::expandRetryMatrix(uint32_t rowstoadd)
{
  //use random numbers to expand the retry matrix by a given number of rows.

  uint32_t initnrows = itsRetryFctr.shape()(0);
  uint32_t npars = itsRetryFctr.shape()(1);

  Matrix<T> rt(initnrows + rowstoadd, npars);
 
  for (uint32_t r = 0; r < initnrows; r++) {
    for (uint32_t p = 0; p < npars; p++) {
      rt(r,p) = itsRetryFctr(r,p);
    }
  }

  Time tmptime(1982,8,31,10);
  MLCG gen(int32_t(tmptime.age()));
  Uniform fgen(&gen, 0.0, 1.0);

  for (uint32_t r = initnrows; r < initnrows + rowstoadd; r++)
  {
    if (itsDimension == 1)
    {
      rt(r,0) = 1;  rt(r,1) = 0;  rt(r,2) = fgen() + 0.5;     
    }
    if (itsDimension == 2)
    {
      rt(r,0) = 1;  rt(r,1) = 0; rt(r,2) = 0; 
      rt(r,3) = fgen() + 0.5;  rt(r,4) = fgen() * 0.7 + 0.3;   
      rt(r,5) = fgen() - 0.5;
    }
    if (itsDimension == 3)
    {
      rt(r,0) = 1;  rt(r,1) = 0;  rt(r,2) = 0;  rt(r,3) = 0;
      rt(r,4) = fgen() + 0.5;  rt(r,5) = fgen() + 0.5;  rt(r,6) = fgen() + 0.5;
      rt(r,7) = fgen() - 0.5;  rt(r,8) = fgen() - 0.5;
    }
  }

  itsRetryFctr.resize();
  itsRetryFctr = rt;
}


template <class T>
uint32_t FitGaussian<T>::countFreeParameters()
{
  uint32_t nfreepars = 0;

  for (uint32_t g = 0; g < itsNGaussians; g++) {
    for (uint32_t p = 0; p < itsDimension*3; p++) {
      if (!itsMask(g,p)) nfreepars++;
    }
  }

  return nfreepars;
}

} //# NAMESPACE CASACORE - END


#endif
