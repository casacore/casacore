#ifndef AIPS_FITGAUSSIAN_H
#define AIPS_FITGAUSSIAN_H

#include <aips/aips.h>
#include <aips/Arrays/Matrix.h>

//A very simple (and relatively crude) class that performs Gaussian fitting
//on a set of data using NonLinearFitLM.  The user specifies the dimension, 
//number of gaussians, initial estimate, retry factors, and the data,
//and the fitting proceeds automatically.  Upon failure of the fitter it will 
//retry the fit according to the retry factors.

//IMPR: as mentioned in the .cc file, the retry system could be much more
//sophisticated. 

template <class T>
class FitGaussian
{
  public:

  FitGaussian();
  FitGaussian(uInt dimension);
  FitGaussian(uInt dimension, uInt numgaussians);

  //parameter fixing, etc.

  void setDimensions(uInt dimensions);
  void setNumGaussians(uInt numgaussians);
  void setFirstEstimate(const Matrix<T>& estimate);
  void setRetryFactors(const Matrix<T>& retryfactors);
  Bool &mask(uInt gaussian, uInt parameter);
  const Bool &mask(uInt gaussian, uInt parameter) const;


  Matrix<T> fit(const Matrix<T>& pos, const Vector<T>& f,
                T chisqcriteria = 5e-5, uInt maxiter = 1024, 
                T convcriteria = 0.0001);
  Matrix<T> fit(const Matrix<T>& pos,const Vector<T>& f,
                const Vector<T>& sigma,
                T chisqcriteria = 5e-5, uInt maxiter = 1024, 
                T convcriteria = 0.0001);

  T chisquared();
  Bool converged();

  private:
  uInt dimension;              // how many dimensions(1,2,or 3)
  uInt ngaussians;             // number of gaussians to fit
  uInt ngpar;                  // const; parameters per gaussian (9)
  Int retries;                 // retries per targetlist
  T chisquare;                 // chisquare of fit
  Bool success;                // flags success or failure

  Matrix<T> firstestimate;
  Matrix<T> startparameters;
  Matrix<T> retryfctr;
  Matrix<Bool> themask;
};


#endif








