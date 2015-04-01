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
//#
//#
//# $Id$

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
FitGaussian<T>::FitGaussian(uInt dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uInt dimensions) - "
                    "dimensions must be 1, 2, or 3"));

  itsDimension = dimensions;
  itsNGaussians = 0;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsSuccess = 0;
}

template <class T>
FitGaussian<T>::FitGaussian(uInt dimensions, uInt numgaussians)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uInt dimensions, "
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
void FitGaussian<T>::setDimensions(uInt dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::setDimenions(uInt dimensions)"
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
void FitGaussian<T>::setNumGaussians(uInt numgaussians)
{
  itsNGaussians = numgaussians;
  itsMaxRetries = 0;
  itsMaxTime = C::dbl_max;
  itsRetryFctr.resize();
  itsFirstEstimate.resize();
  itsMask.resize();
  if (itsDimension*3 && itsNGaussians) {
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
Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter)
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsDimension*3))
    throw(AipsError("FitGaussian<T>::mask(uInt gaussian, uInt parameter)"
                    " - index out of range"));
  return itsMask(gaussian, parameter);
}

template <class T>
const Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter) const
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsDimension*3))
    throw(AipsError("FitGaussian<T>::mask(uInt gaussian, uInt parameter"
                    " const - index out of range"));
  return itsMask(gaussian, parameter);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                               T maximumRMS, uInt maxiter, 
                               T convcriteria)
{
  //Same as below, with all sigma = 1.

  Vector<T> sigma(f.nelements(), 1);

  return fit(pos, f, sigma, maximumRMS, maxiter, convcriteria);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                              const Vector<T>& sigma, T maximumRMS,
                              uInt maxiter, T convcriteria)
{
  //Perform the fitting to the data.  Sets up NonLinearFitLM with the specified
  //number of gaussians and starts fitting.  If the fit fails or converges
  //with an RMS above maximumRMS, it retries by multiplying certain
  //estimate gaussians by the retry matrix.

  uInt const ngpars = itsDimension*3;

  if (pos.ncolumn() != itsDimension) 
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uInt maxiter, T convcriteria) - "
                    " pos is of wrong number of dimensions."));

  if ((pos.nrow() != f.nelements()) || (pos.nrow() != sigma.nelements()))
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uInt maxiter, T convcriteria) - "
                    " pos, f, and sigma must all have same length."));

  if (pos.nrow() <= 0)
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T maximumRMS,"
                    " uInt maxiter, T convcriteria) - "
                    " pos contains no data."));



  NonLinearFitLM<T> fitter(0);
  Vector<T> solution;
  Matrix<T> startparameters(itsNGaussians, ngpars);
  Matrix<T> solutionparameters(itsNGaussians, ngpars);

 Block<Gaussian1D<AutoDiff<T> > > gausscomp1d((itsDimension==1)*itsNGaussians);
 Block<Gaussian2D<AutoDiff<T> > > gausscomp2d((itsDimension==2)*itsNGaussians);
 Block<Gaussian3D<AutoDiff<T> > > gausscomp3d((itsDimension==3)*itsNGaussians);

  fitter.setMaxIter(maxiter);
  fitter.setCriteria(convcriteria);
 
  Vector<Int> targetmask(itsNGaussians,-1); //should rename this... 
  uInt attempt = 0;                         //overall attempt number
  Int fitfailure;
  T bestRMS = C::flt_max;  //how to template this properly...
  
  itsSuccess = 0; 

  if (itsMaxRetries > 0 && nRetryFactors() == 0) {
    setRetryFactors();
  }

  //If there are not enough data points, fix some parameters to the estimate

  if (itsNGaussians >= pos.nrow())
  {
    for (uInt p = 1; p < ngpars; p++) {
      for (uInt g = 0; g < itsNGaussians; g++) {
        mask(g,p) = 0;
      }
    }
    if (itsNGaussians > pos.nrow()) {
      uInt g = 0;
      while (countFreeParameters() > pos.nrow()) {
        mask(g,0) = 0;
        g++;
      }
    }
  }

  uInt fixpar = ngpars;
  while (countFreeParameters() > pos.nrow()) {
    fixpar--;
    if (fixpar == itsDimension * 2) fixpar = itsDimension; //fix widths last
    if (fixpar == 0) fixpar = itsDimension * 2; 
    for (uInt g = 0; g < itsNGaussians; g++) {
      mask(g,fixpar) = 1;
    }
  }


  //Begin fitting
  
  Timer timer;
  timer.mark();

  do {   

    // Modify the estimate according to the retry factors, if necessary.

    if ((attempt) && (attempt <= itsMaxRetries)) {
      if (pow(Int(nRetryFactors()),Int(itsNGaussians)) <Int(itsMaxRetries)*3/2)
      {
        //Eventual redundancy is very likely, so make the retry matrix bigger.
        expandRetryMatrix(1);
      }

      Time tmptime(1982,8,31,10);
      MLCG gen(Int(tmptime.age()));
      //DiscreteUniform retgen(&gen, -nRetryFactors(), nRetryFactors()-1);
      // any negative number means use the unaltered estimate (50% chance)
   
      //The new (2002/07/11) retry system is very simple: the retry targets
      //are chosen at random, as is the selection from the retry matrix.

      uInt ntargets = (gen.asuInt() % (1 + itsNGaussians / 2)) + 1;

      targetmask = -1;
      for (uInt i = 0; i < ntargets; i++) {
        uInt t = gen.asuInt() % itsNGaussians;
        targetmask(t) = Int(gen.asuInt() % nRetryFactors());
      }     
    }

    //cout << targetmask << endl;

    // Set the initial estimate and create the component gaussian functionals
    // used in fitting.
    
    for (uInt g = 0; g < itsNGaussians; g++) {
      for (uInt p = 0; p < ngpars; p++) {
        startparameters(g,p) = itsFirstEstimate(g,p);
        if (targetmask(g) >= 0) {
          //apply retry factors
          Int retry = targetmask(g);
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
    for (uInt g = 0; g < itsNGaussians; g++) {
      if (itsDimension==1) sumfunc.addFunction(gausscomp1d[g]);
      if (itsDimension==2) sumfunc.addFunction(gausscomp2d[g]);
      if (itsDimension==3) sumfunc.addFunction(gausscomp3d[g]);
    }

    fitter.setFunction(sumfunc);  //sumgauss
    fitter.setCriteria(convcriteria);

    solution.resize(0);
    fitfailure = 0;
    attempt++;

    cout << "Attempt " << attempt << ": ";
  
    // Perform the fit, and check for problems with the results.
    
    try {
       solution = fitter.fit(pos, f, sigma);
    } catch (AipsError fittererror) {
      string errormessage;
      errormessage = fittererror.getMesg();
      cout << "Unsuccessful - Error during fitting." << endl;
      cout << errormessage << endl;
      fitfailure = 2;
    } 
    if (!fitter.converged() && !fitfailure) {
      fitfailure = 1;
      cout << "Unsuccessful - Failed to converge." << endl;
    }
    if (fitter.converged()) {
      itsChisquare = fitter.chiSquare();
      if (itsChisquare < 0) {
        cout << "Unsuccessful - ChiSquare of "<< itsChisquare << "is negative."
             << endl;
        fitfailure = 3;
      }
      else if (isNaN(itsChisquare)){
        cout << "Unsuccessful - Convergence to NaN result" << endl;
        fitfailure = 3;
      }
      else {

        for (uInt g = 0; g < itsNGaussians; g++) {    
          if ((itsDimension == 1  &&  solution(g*ngpars+2) < 0)   ||
	      (itsDimension == 2  && (solution(g*ngpars+3) < 0  || 
				      solution(g*ngpars+4) < 0))  ||
              (itsDimension == 3  && (solution(g*ngpars+4) < 0  || 
	                              solution(g*ngpars+5) < 0  ||
                                      solution(g*ngpars+6) < 0))) { 
            fitfailure = 4;
            cout << "Unsuccessful - Negative axis widths not permissible.";
            cout << endl;
	    break;
	  }
	}

        if (!fitfailure) {
          itsRMS = sqrt(itsChisquare / f.nelements());
          if (itsRMS > maximumRMS) {
            cout << "Unsuccessful - RMS of " << itsRMS;
            cout << " is outside acceptible limits." << endl;
            fitfailure = 5;
          }
          else
	  {
            cout << "Converged after " << fitter.currentIteration() 
                 << " iterations" << endl;
	  }

	  if (itsRMS < bestRMS) { 
            //best fit so far - write parameters to solution matrix
            for (uInt g = 0; g < itsNGaussians; g++) {  
              for (uInt p = 0; p < ngpars; p++) {
                solutionparameters(g,p) = solution(g*ngpars+p);
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
        cout << "Retry limit reached, ";
      }
      else if (timer.real() >= itsMaxTime) { 
        cout << "Time limit reached, ";
      }
      cout << "no fit satisfies RMS criterion; using best available fit";
      cout << endl;
    }
    correctParameters(solutionparameters);
    return solutionparameters;
  }

// Otherwise, return all zeros 
 
  cout << "FAILURE - could not find acceptible convergent solution." << endl;
  itsSuccess = 0;

  for (uInt g = 0; g < itsNGaussians; g++)  {   
    for (uInt p = 0; p < ngpars; p++) {
      solutionparameters(g,p) = T(0.0);
    }
  }
//
  return solutionparameters;
   
}

template <class T>
void FitGaussian<T>::correctParameters(Matrix<T>& parameters)
{
  //bring rotation/axis values into the stated domain.

  for (uInt g = 0; g < itsNGaussians; g++) {     
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
Bool FitGaussian<T>::converged()
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
void FitGaussian<T>::expandRetryMatrix(uInt rowstoadd)
{
  //use random numbers to expand the retry matrix by a given number of rows.

  uInt initnrows = itsRetryFctr.shape()(0);
  uInt npars = itsRetryFctr.shape()(1);

  Matrix<T> rt(initnrows + rowstoadd, npars);
 
  for (uInt r = 0; r < initnrows; r++) {
    for (uInt p = 0; p < npars; p++) {
      rt(r,p) = itsRetryFctr(r,p);
    }
  }

  Time tmptime(1982,8,31,10);
  MLCG gen(Int(tmptime.age()));
  Uniform fgen(&gen, 0.0, 1.0);

  for (uInt r = initnrows; r < initnrows + rowstoadd; r++)
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
uInt FitGaussian<T>::countFreeParameters()
{
  uInt nfreepars = 0;

  for (uInt g = 0; g < itsNGaussians; g++) {
    for (uInt p = 0; p < itsDimension*3; p++) {
      if (!itsMask(g,p)) nfreepars++;
    }
  }

  return nfreepars;
}

} //# NAMESPACE CASACORE - END


#endif
