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

#define PI C::pi
#define PI_2 C::pi_2
#define PI_4 C::pi_4

template <class T>
FitGaussian<T>::FitGaussian()
{
  dimension = 0;
  ngaussians = 0;
  ngpar = 0;
  retries = 0;
}

template <class T>
FitGaussian<T>::FitGaussian(uInt dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uInt dimensions) - "
                    "dimensions must be 1, 2, or 3"));

  dimension = dimensions;
  ngaussians = 0;
  ngpar = dimension * 3;
  retries = 0;
}

template <class T>
FitGaussian<T>::FitGaussian(uInt dimensions, uInt numgaussians)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::FitGaussian(uInt dimensions, "
                    "uInt numgaussians) - dimensions must be 1, 2, or 3"));
 
  dimension = dimensions;
  ngaussians = numgaussians;
  ngpar = dimension * 3;
  themask.resize(ngaussians, ngpar);
  themask = 1;
  retries = 0;
}

template <class T>
void FitGaussian<T>::setDimensions(uInt dimensions)
{
  if ((dimensions == 0) || (dimensions > 3))
    throw(AipsError("FitGaussian<T>::setDimenions(uInt dimensions)"
                    " - dimensions must be 1, 2, or 3")); 
  dimension = dimensions;
  ngpar = dimension * 3;
  retries = 0;
  if (ngaussians) {themask.resize(ngaussians, ngpar); themask = 1;}
}

template <class T>
void FitGaussian<T>::setNumGaussians(uInt numgaussians)
{
  ngaussians = numgaussians;
  retries = 0;
  if (ngpar && ngaussians) {themask.resize(ngaussians, ngpar); themask = 1;}
}

template <class T>
void FitGaussian<T>::setFirstEstimate(const Matrix<T>& estimate)
{
  if ((estimate.nrow() != ngaussians) || (estimate.ncolumn() != ngpar))
    throw(AipsError("FitGaussian<T>::setfirstestimate(const Matrix<T>& "
                    "estimate) - estimate must be of shape "
                    "[(ngaussians) , (dimension x 3)]"));

  firstestimate.resize();
  firstestimate = estimate;
}

template <class T>
void FitGaussian<T>::setRetryFactors(const Matrix<T>& retryfactors)
{

  if (retryfactors.ncolumn() != ngpar)
    throw(AipsError("FitGaussian<T>::setretryfactors(const Matrix<T>&"
                    " retryfactors) - retryfactors must have numcolumns = "
                    " dimension x 3"));
 
  retryfctr.resize(); 
  retryfctr = retryfactors;
  retries = retryfctr.nrow();
}

template <class T>
Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter)
{
  if ((gaussian >= ngaussians) || (parameter >= ngpar))
    throw(AipsError("FitGaussian<T>::mask(uInt gaussian, uInt parameter)"
                    " - index out of range"));
  return themask(gaussian, parameter);
}

template <class T>
const Bool &FitGaussian<T>::mask(uInt gaussian, uInt parameter) const
{
  if ((gaussian >= ngaussians) || (parameter >= ngpar))
    throw(AipsError("FitGaussian<T>::mask(uInt gaussian, uInt parameter"
                    " const - index out of range"));
  return themask(gaussian, parameter);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                               T chisqcriteria, uInt maxiter, 
                               T convcriteria)
{
  //Same as below, with all sigma = 1.

  Vector<T> sigma(f.nelements(), 1);

  return fit(pos, f, sigma, chisqcriteria, maxiter, convcriteria);
}

template <class T>
Matrix<T> FitGaussian<T>::fit(const Matrix<T>& pos, const Vector<T>& f,
                              const Vector<T>& sigma,T chisqcriteria,
                              uInt maxiter, T convcriteria)
{
  //Perform the fitting to the data.  Sets up NonLinearFitLM with the specified
  //number of gaussians and starts fitting.  If the fit fails or converges
  //with a value above chisqcriteria, it retries by multiplying certain
  //estimate gaussians by the retry matrix.

  if (ngaussians * ngpar > pos.nrow()) 
    cout << "WARNING: " << ngaussians*ngpar << " parameters with only "
         << pos.nrow() << " data points" << endl;

  if (pos.ncolumn() != dimension) 
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T chisqcriteria ="
                    " 5e-5, uInt maxiter = 1024, T convcriteria = 0.0001) - "
                    " pos is of wrong number of dimensions."));

  if ((pos.nrow() != f.nelements()) || (pos.nrow() != sigma.nelements()))
    throw(AipsError("FitGaussian<T>::fit(const Matrix<T>& pos, const"
                    " Vector<T>& f, const Vector<T>& sigma, T chisqcriteria ="
                    " 5e-5, uInt maxiter = 1024, T convcriteria = 0.0001) - "
                    " pos, f, and sigma must all have same length."));

  if (chisqcriteria <= 0) chisqcriteria = 5e-5;

  NonLinearFitLM<T> fitter(0);
  Vector<T> solution;
  Matrix<T> startparameters(ngaussians, ngpar);
  Matrix<T> solutionparameters(ngaussians, ngpar);

  Block<Gaussian1D<AutoDiff<T> > > gausscomp1d((dimension==1)*ngaussians);
  Block<Gaussian2D<AutoDiff<T> > > gausscomp2d((dimension==2)*ngaussians);
  Block<Gaussian3D<AutoDiff<T> > > gausscomp3d((dimension==3)*ngaussians);

  //AutoDiff<T>(1);

  fitter.setMaxIter(maxiter);
  fitter.setCriteria(convcriteria);
 
  //A 'target' is a gaussian currently being modified by the retrymatrix.  

  Int retry = retries-1;                  //try number for current target list
  Int targetattempt = 0;                  //number unique target lists tried
  uInt ntargets = 0;                      //number of targets currently active
  Vector<Bool> targetmask(ngaussians,0);  //list of targets to apply retryfctr
  Int attempt = 0;                        //overall attempt number
  Int fitfailure;
  

  do {   

// Set the initial estimate and create the component gaussian functionals
// used in fitting.
    
    for (uInt g = 0; g < ngaussians; g++) {
      for (uInt p = 0; p < ngpar; p++) {
        startparameters(g,p) = firstestimate(g,p);
        if (targetmask(g)) {
          //apply retry factors
          if (dimension == 1) {
            if (p == 1) startparameters(g,p) += retryfctr(retry,p);
            else        startparameters(g,p) *= retryfctr(retry,p);
	  }
          if (dimension == 2) {
	    if ((p == 1) || (p == 2) || (p == 5)) {
              startparameters(g,p) += retryfctr(retry,p);
            } else {
              startparameters(g,p) *= retryfctr(retry,p);
            }
	  }
          if (dimension == 3) {
	    if ((p == 1) || (p == 2) || (p == 3) || (p == 7) || (p == 8)) {
              startparameters(g,p) += retryfctr(retry,p);
            } else {
              startparameters(g,p) *= retryfctr(retry,p);
            }
	  }
	}
        if (dimension==1) { 
          gausscomp1d[g][p]=AutoDiff<T>(startparameters(g,p), ngpar, p);
          gausscomp1d[g].mask(p) = themask(g,p);
	}
        if (dimension==2) {
          gausscomp2d[g][p]=AutoDiff<T>(startparameters(g,p), ngpar, p);
          gausscomp2d[g].mask(p) = themask(g,p);
	}
        if (dimension==3) {
          gausscomp3d[g][p]=AutoDiff<T>(startparameters(g,p), ngpar, p);
          gausscomp3d[g].mask(p) = themask(g,p);
	}
      }
    }

// Create the fitting function by summing up the component gaussians.
   
    CompoundFunction<AutoDiff<T> > sumfunc;
    for (uInt g = 0; g < ngaussians; g++) {
      if (dimension==1) sumfunc.addFunction(gausscomp1d[g]);
      if (dimension==2) sumfunc.addFunction(gausscomp2d[g]);
      if (dimension==3) sumfunc.addFunction(gausscomp3d[g]);
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
      chisquare = fitter.chiSquare();
      if (chisquare > chisqcriteria) {
        cout << "Failure - ChiSquare of " << chisquare;
        cout << " is outside acceptible limits." << endl;
        fitfailure = 3;
      }
      if (chisquare < 0) {
        cout << "Failure - ChiSquare of " << chisquare << "is negative."<<endl;
        fitfailure = 3;
      }
      if (isNaN(chisquare)){
        cout << "Failure - Convergence to NaN result" << endl;
        fitfailure = 3;
      }
      if (!fitfailure) {
        for (uInt g = 0; g < ngaussians; g++)  {   //write parameters to sol matx
          for (uInt p = 0; p < ngpar; p++) {
            solutionparameters(g,p) = solution(g*ngpar+p);
          }
        }

        for (uInt g = 0; g < ngaussians; g++) {
           
          if ((dimension == 1) &&  (solutionparameters(g,2) < 0)   ||
	      (dimension == 2) && ((solutionparameters(g,3) < 0) || 
				   (solutionparameters(g,4) < 0))  ||
              (dimension == 3) && ((solutionparameters(g,4) < 0) || 
	                           (solutionparameters(g,5) < 0) ||
                                   (solutionparameters(g,6) < 0))) { 
            fitfailure = 4;
            cout << "Failure - Negative axis widths not permissible.";
            cout << endl;
	    break;
	  }
	}
      }
    }
    
    if (!retries) break; //skip retry procedure if no retries requested
 
    if (++retry == retries)  {

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
      for (uInt t = ngaussians-1; (t > 0) && !endloop; t--) {
        if (targetmask(t-1) && !targetmask(t)){
          targetmask(t-1) = 0; //shift digit to right: 00100 -> 00010
          targetmask(t) = 1;
          endloop = 1;
	}
      }
      if (!endloop) {
        ntargets++;
        if (ntargets <= ngaussians)  {   //new target: shuffle all 1s to left
          for (uInt t2 = 0; t2 < ntargets; t2++) targetmask(t2) = 1;
          for (uInt t2 = ntargets; t2 < ngaussians; t2++) targetmask(t2) = 0;
	}
      }
      targetattempt++;
    }

  } while ((fitfailure) && (ntargets <= ngaussians));

//
  if (!fitfailure)  {
    cout << "Converged after " << fitter.currentIteration() 
         << " iterations" << endl;

    for (uInt g = 0; g < ngaussians; g++) {     
      //bring rotation/axis values into standard domain.
      //IMPR: This should be a separate function.

      if (dimension == 2) {
        if (solutionparameters(g,4) > 1) {
          solutionparameters(g,3) *= solutionparameters(g,4);
          solutionparameters(g,4) = 1/solutionparameters(g,4);      //swap axes
          solutionparameters(g,5) += PI/2;
        }
        while (solutionparameters(g,5) < 0) solutionparameters(g,5) += PI;
        while (solutionparameters(g,5) > PI) solutionparameters(g,5) -= PI;
      }
      if (dimension == 3) {  
        while (solutionparameters(g,7) < -PI_2) solutionparameters(g,7) += PI;
        while (solutionparameters(g,7) > PI_2) solutionparameters(g,7) -= PI;

        while (solutionparameters(g,8) < -PI_2) solutionparameters(g,8) += PI;
        while (solutionparameters(g,8) > PI_2) solutionparameters(g,8) -= PI;

        if (abs(solutionparameters(g,7)) > PI_4) {
          //swap y/x axes
          T temp = solutionparameters(g,4);
          solutionparameters(g,4) = solutionparameters(g,5);
          solutionparameters(g,5) = temp;
          if (solutionparameters(g,7) > 0)
            solutionparameters(g,7) -= PI_2;
          else
            solutionparameters(g,7) += PI_2;
	}
        if (abs(solutionparameters(g,8)) > PI_4) {
          //swap z/x axes
          T temp = solutionparameters(g,4);
          solutionparameters(g,4) = solutionparameters(g,6);
          solutionparameters(g,6) = temp;
          if (solutionparameters(g,8) > 0) {
            solutionparameters(g,8) -= PI_2;
          } else {
            solutionparameters(g,8) += PI_2;
          }
	}
      }
    }
    success = 1;
    return solutionparameters;
  }
 
  cout << "FAILURE - could not find acceptible convergent solution." << endl;
  success = 0;

  for (uInt g = 0; g < ngaussians; g++)  {   //write parameters to sol matx
    for (uInt p = 0; p < ngpar; p++) {
      solutionparameters(g,p) = solution(g*ngpar+p);
    }
  }
//
  return solutionparameters;
   
}

template <class T>
Bool FitGaussian<T>::converged()
{
  //Did the fitter converge to an acceptible value?
  return success;
}

template <class T>
T FitGaussian<T>::chisquared()
{
  //Chisquared of completed fit   IMPR: shouldn't work if no convergence?
  return chisquare;
}








