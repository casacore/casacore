//                              FitGaussian.cc

#include <trial/Fitting/FitGaussian.h>

#include <trial/Fitting/NonLinearFitLM.h>
#include <aips/Mathematics/AutoDiffIO.h>
#include <aips/Functionals/CompoundFunction.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Gaussian2D.h>
#include <trial/Functionals/Gaussian3D.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>

#include <aips/Exceptions/Error.h>
#include <aips/iostream.h>


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
  itsNGPars = 0;
  itsRetries = 0;
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
  itsNGPars = itsDimension * 3;
  itsRetries = 0;
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
  itsNGPars = itsDimension * 3;
  itsMask.resize(itsNGaussians, itsNGPars);
  itsMask = 1;
  itsRetries = 0;
  itsSuccess = 0;
}

template <class T>
void FitGaussian<T>::setDimensions(uInt dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::setDimenions(uInt dimensions)"
                    " - dimensions must be 1, 2, or 3")); 
  itsDimension = dimensions;
  itsNGPars = itsDimension * 3;
  itsRetries = 0;
  if (itsNGaussians) {
    itsMask.resize(itsNGaussians, itsNGPars); itsMask = 1;
  }
}

template <class T>
void FitGaussian<T>::setNumGaussians(uInt numgaussians)
{
  itsNGaussians = numgaussians;
  itsRetries = 0;
  if (itsNGPars && itsNGaussians) {
    itsMask.resize(itsNGaussians, itsNGPars); itsMask = 1;
  }
}

template <class T>
void FitGaussian<T>::setFirstEstimate(const Matrix<T>& estimate)
{
  if ((estimate.nrow() != itsNGaussians) || (estimate.ncolumn() != itsNGPars))
    throw(AipsError("FitGaussian<T>::setfirstestimate(const Matrix<T>& "
                    "estimate) - estimate must be of shape "
                    "[(ngaussians) , (dimension x 3)]"));

  itsFirstEstimate.resize();
  itsFirstEstimate = estimate;
}

template <class T>
void FitGaussian<T>::setRetryFactors(const Matrix<T>& retryfactors)
{

  if (retryfactors.ncolumn() != itsNGPars)
    throw(AipsError("FitGaussian<T>::setretryfactors(const Matrix<T>&"
                    " retryfactors) - retryfactors must have numcolumns = "
                    " dimension x 3"));
 
  itsRetryFctr.resize(); 
  itsRetryFctr = retryfactors;
  itsRetries = itsRetryFctr.nrow();
}

template <class T>
Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter)
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsNGPars))
    throw(AipsError("FitGaussian<T>::mask(uInt gaussian, uInt parameter)"
                    " - index out of range"));
  return itsMask(gaussian, parameter);
}

template <class T>
const Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter) const
{
  if ((gaussian >= itsNGaussians) || (parameter >= itsNGPars))
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

  if (itsNGaussians * itsNGPars > pos.nrow()) 
    cout << "WARNING: " << itsNGaussians*itsNGPars << " parameters with only "
         << pos.nrow() << " data points" << endl;

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


  NonLinearFitLM<T> fitter(0);
  Vector<T> solution;
  Matrix<T> itsStartParameters(itsNGaussians, itsNGPars);
  Matrix<T> solutionparameters(itsNGaussians, itsNGPars);

 Block<Gaussian1D<AutoDiff<T> > > gausscomp1d((itsDimension==1)*itsNGaussians);
 Block<Gaussian2D<AutoDiff<T> > > gausscomp2d((itsDimension==2)*itsNGaussians);
 Block<Gaussian3D<AutoDiff<T> > > gausscomp3d((itsDimension==3)*itsNGaussians);

  //AutoDiff<T>(1);

  fitter.setMaxIter(maxiter);
  fitter.setCriteria(convcriteria);
 
  //A 'target' is a gaussian currently being modified by the retrymatrix.  

  Int retry = itsRetries-1;                  //try number for current target list
  Int targetattempt = 0;                  //number unique target lists tried
  uInt ntargets = 0;                      //number of targets currently active
  Vector<Bool> targetmask(itsNGaussians,0);//list of targets to apply retryfctr
  Int attempt = 0;                        //overall attempt number
  Int fitfailure;
  T bestRMS = C::flt_max;  //how to template this properly...
  
  itsSuccess = 0; 

  do {   

// Set the initial estimate and create the component gaussian functionals
// used in fitting.
    
    for (uInt g = 0; g < itsNGaussians; g++) {
      for (uInt p = 0; p < itsNGPars; p++) {
        itsStartParameters(g,p) = itsFirstEstimate(g,p);
        if (targetmask(g)) {
          //apply retry factors
          if (itsDimension == 1) {
            if (p == 1) itsStartParameters(g,p) += itsRetryFctr(retry,p);
            else        itsStartParameters(g,p) *= itsRetryFctr(retry,p);
	  }
          if (itsDimension == 2) {
	    if ((p == 1) || (p == 2) || (p == 5)) {
              itsStartParameters(g,p) += itsRetryFctr(retry,p);
            } else {
              itsStartParameters(g,p) *= itsRetryFctr(retry,p);
            }
	  }
          if (itsDimension == 3) {
	    if ((p == 1) || (p == 2) || (p == 3) || (p == 7) || (p == 8)) {
              itsStartParameters(g,p) += itsRetryFctr(retry,p);
            } else {
              itsStartParameters(g,p) *= itsRetryFctr(retry,p);
            }
	  }
	}
        if (itsDimension==1) { 
          gausscomp1d[g][p]=AutoDiff<T>(itsStartParameters(g,p), itsNGPars, p);
          gausscomp1d[g].mask(p) = itsMask(g,p);
	}
        if (itsDimension==2) {
          gausscomp2d[g][p]=AutoDiff<T>(itsStartParameters(g,p), itsNGPars, p);
          gausscomp2d[g].mask(p) = itsMask(g,p);
	}
        if (itsDimension==3) {
          gausscomp3d[g][p]=AutoDiff<T>(itsStartParameters(g,p), itsNGPars, p);
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
  
    
    try {
       solution = fitter.fit(pos, f, sigma);
    } catch (AipsError fittererror) {
      string errormessage;
      errormessage = fittererror.getMesg();
      cout << "Failure - Error during fitting." << endl;
      cout << errormessage << endl;
      fitfailure = 2;
    } 
    if (!fitter.converged() && !fitfailure) {
      fitfailure = 1;
      cout << "Failure - Failed to converge." << endl;
    }
    if (fitter.converged()) {
      itsChisquare = fitter.chiSquare();
      if (itsChisquare < 0) {
        cout << "Failure - ChiSquare of " << itsChisquare << "is negative."<<endl;
        fitfailure = 3;
      }
      else if (isNaN(itsChisquare)){
        cout << "Failure - Convergence to NaN result" << endl;
        fitfailure = 3;
      }
      else {

        for (uInt g = 0; g < itsNGaussians; g++) {    
          if ((itsDimension == 1) &&  (solution(g*itsNGPars+2) < 0)   ||
	      (itsDimension == 2) && ((solution(g*itsNGPars+3) < 0) || 
				      (solution(g*itsNGPars+4) < 0))  ||
              (itsDimension == 3) && ((solution(g*itsNGPars+4) < 0) || 
	                              (solution(g*itsNGPars+5) < 0) ||
                                      (solution(g*itsNGPars+6) < 0))) { 
            fitfailure = 4;
            cout << "Failure - Negative axis widths not permissible.";
            cout << endl;
	    break;
	  }
	}

        if (!fitfailure) {
          itsRMS = sqrt(itsChisquare / f.nelements());
          if (itsRMS > maximumRMS) {
            cout << "Failure - RMS of " << itsRMS;
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
              for (uInt p = 0; p < itsNGPars; p++) {
                solutionparameters(g,p) = solution(g*itsNGPars+p);
              }
            }
            bestRMS = itsRMS;
            itsSuccess = 1;   //even if it's not a complete success
	  }

	}

      }
    }
    
    if (!itsRetries) break; //skip retry procedure if no retries requested
 
    if (++retry == itsRetries)  {

// This controls the retry system.  If the first fit fails, the retry
// matrix is applied to the first gaussian only, then the second only,
// then the third only, etc.   Then it's applied to the first and second
// only, first and third only.... second and third only... first and
// second and third only... and so on, until every combination has
// been exhausted.

      //IMPR: since the number of retries increases exponentially with the
      //number of gaussians, it would be a good idea to skip some when
      //ngaussians is large.

      retry = 0;

      //cout << targetattempt << ": " << targetmask << endl;

      Bool endloop = 0;
      for (uInt t = itsNGaussians-1; (t > 0) && !endloop; t--) {
        if (targetmask(t-1) && !targetmask(t)){
          targetmask(t-1) = 0; //shift digit to right: 00100 -> 00010
          targetmask(t) = 1;
          endloop = 1;
	}
      }
      if (!endloop) {
        ntargets++;
        if (ntargets <= itsNGaussians)  {  //new target: shuffle all 1s to left
          for (uInt t2 = 0; t2 < ntargets; t2++) targetmask(t2) = 1;
          for (uInt t2 = ntargets; t2 < itsNGaussians; t2++) targetmask(t2)=0;
	}
      }
      targetattempt++;
    }

  } while ((fitfailure) && (ntargets <= itsNGaussians));

// If at least one convergent solution has been found, return its parameters

  if (itsSuccess) {
    if (fitfailure) {
      cout << "No fit satisfies RMS criterion; using best available fit";
      cout << endl;
    }
    correctParameters(solutionparameters);
    return solutionparameters;
  }

// Otherwise, return all zeros 
 
  cout << "FAILURE - could not find acceptible convergent solution." << endl;
  itsSuccess = 0;

  for (uInt g = 0; g < itsNGaussians; g++)  {   
    for (uInt p = 0; p < itsNGPars; p++) {
      solutionparameters(g,p) = T(0.0);
    }
  }
//
  return solutionparameters;
   
}

template <class T>
void FitGaussian<T>::correctParameters(Matrix<T>& parameters)
{
  //bring rotation/axis values into standard domain.

  //check dimensionality?

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






