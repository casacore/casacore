//# FitGaussian.h: Multidimensional fitter class for Gaussians
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
#ifndef AIPS_FITGAUSSIAN_H
#define AIPS_FITGAUSSIAN_H

#include <aips/aips.h>
#include <aips/Arrays/Matrix.h>

// <summary>Multidimensional fitter class for Gaussians.</summary>

// <reviewed reviewer="" date="" tests="tFitGaussian">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Gaussian1D">Gaussian1D</linkto> class
//   <li> <linkto class="Gaussian2D">Gaussian2D</linkto> class
//   <li> <linkto class="Gaussian3D">Gaussian3D</linkto> class
//   <li> <linkto class="NonLinearFitLM">NonLinearFitLM</linkto> class
// </prerequisite>

// <etymology>
// Fits Gaussians to data.
// </etymology>

// <synopsis>

// <src>FitGaussian</src> is specially designed for fitting procedures in
// code that must be generalized for general dimensionality and 
// number of components, and for complicated fits where the failure rate of
// the standard nonlinear fitter is unacceptibly high (which, for only four
// components, can by as much as 90% in noiseless data.)

// <src>FitGaussian</src> essentially provides a Gaussian-adapted 
// interface for NonLinearFitLM.  The user specifies the dimension, 
// number of gaussians, initial estimate, retry factors, and the data,
// and the fitting proceeds automatically.  Upon failure of the fitter it will 
// retry the fit according to the retry factors until a fit is completed
// successfully.  The user can optionally require as a criterion for success
// that the RMS (root-mean-square, sqrt[chisq/N], an estimate of the average
// error) of the fit not exceed some maximum value.

// The retry factors are applied in different ways: the height and widths
// are multiplied by the retry factors while the center and angles are
// increased by their factors.  The algorithm for applying these factors is
// rather inefficient.  After the initial estimate fails, the first row of 
// factors are applied to the first gaussian.  If that fails they are applied
// to the second (but not the first), then the third (and no other), etc.
// If applying the factors to all gaussians singly fails in all cases, they
// are applied to two gaussians at once for every possible combination; then
// three gaussians, and so on.  When all combinations are tried the second row
// of factors is brought in to service in the same manner. Obviously this leads
// to a potentially huge number of retries (nrows x 2^ngaussians).  In addition
// the ability to customize retryfactors is rather useless and an automatic
// generation method would be preferable.  Therefore it would be a good idea
// to change the current system, replacing retryfactors with a simple integer
// "maxRetries" parameter to specify the number of total retries.  Making the
// retry system actually sophisticated would be very difficult but simple
// variations on the current system would probably be quite easy.

// </synopsis>

// <example>
// <srcblock>
// FitGaussian<Double> fitgauss(1,1);
// Matrix<Double> x(5,1); x(0,0) = 0; x(1,0) = 1; x(2,0) = 2; x(3,0) = 3; x(4,0) = 4;
// Vector<Double> y(5); y(0) = 0; y(1) = 1; y(2) = 4; y(3) = 1; y(4) = 1;
// Matrix<Double> estimate;
// estimate(0,0) = 1; estimate(0,1) = 1; estimate(0,2) = 1;
// fitgauss.setFirstEstimate(estimate);
// Matrix<Double> solution;
// solution = fitgauss.fit(x,y);
// cout << solution;
// <srcblock>
// I haven't checked that this example actually works, though...
// </example>

// <motivation>
// Fitting multiple Gaussians is required for many different applications,
// but requires a substantial amount of coding - especially if the 
// dimensionality of the image is not known to the programmer.  Furthermore,
// fitting multiple Gaussians has a very high failure rate.  So, a specialized
// Gaussian fitting class that retries from different initial estimates
// until an acceptible fit was found was needed.
// </motivation>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//      implementation only tested for real types (and AutoDiff of them).
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
//    <li> AipsError if estimate/retry matrices are of wrong dimension
//    <li> others?
// </thrown>
  
// <todo asof="2002/06/19">
//   <li> Automatically generate retry matrices
//   <li> Consider using a more sophisticated retry ststem (above).
//   <li> Consider adding other models (polynomial, etc) to make this a Fit3D
//        class.
// </todo>



template <class T>
class FitGaussian
{
  public:

  //# Constructors
  // Create the fitter.  The dimension and the number of gaussians to fit
  // can be modified later if necessary.
  // <group>
  FitGaussian();
  FitGaussian(uInt dimension);
  FitGaussian(uInt dimension, uInt numgaussians);
  // </group>

  // #Member functions
  // Adjust the number of dimensions
  void setDimensions(uInt dimensions);

  // Adjust the number of gaussians to fit
  void setNumGaussians(uInt numgaussians);

  // Set the initial estimate (the starting point of the first fit.)
  void setFirstEstimate(const Matrix<T>& estimate);

  // Set the retry factors, the values that are added/multiplied with the
  // first estimate on subsequent attempts if the first attempt fails.
  void setRetryFactors(const Matrix<T>& retryfactors);

  // Mask out some parameters so that they are not modified during fitting
  Bool &mask(uInt gaussian, uInt parameter);
  const Bool &mask(uInt gaussian, uInt parameter) const;

  // Run the fit, using the data provided in the arguments pos and f.
  // The fit will retry from different initial estimates until it converges
  // to a value with an RMS error less than maximumRMS.  If this cannot be
  // accomplished it will simply take the result that generated the best RMS.
  Matrix<T> fit(const Matrix<T>& pos, const Vector<T>& f,
                T maximumRMS = 1.0, uInt maxiter = 1024, 
                T convcriteria = 0.0001);
  Matrix<T> fit(const Matrix<T>& pos,const Vector<T>& f,
                const Vector<T>& sigma,
                T maximumRMS = 1.0, uInt maxiter = 1024, 
                T convcriteria = 0.0001);

  // Internal function for ensuring that parameters stay within their stated
  // domains (see <src>Gaussian2D</src> and <src>Gaussian3D</src>.)
  void correctParameters(Matrix<T>& parameters);

  // Return the chi squared of the fit
  T chisquared();

  // Return the RMS of the fit
  T RMS();

  // Returns True if the fit (eventually) converged to a value.
  Bool converged();


  private:
  uInt itsDimension;           // how many dimensions(1,2,or 3)
  uInt itsNGaussians;          // number of gaussians to fit
  uInt itsNGPars;              // const; parameters per gaussian (9)
  Int itsRetries;              // retries per targetlist
  T itsChisquare;              // chisquare of fit
  T itsRMS;                    // RMS of fit (sqrt[chisquare / N])
  Bool itsSuccess;             // flags success or failure

  Matrix<T> itsFirstEstimate;
  Matrix<T> itsStartParameters;
  Matrix<T> itsRetryFctr;
  Matrix<Bool> itsMask;        // masks parameters not to change in fitting
};


#endif








