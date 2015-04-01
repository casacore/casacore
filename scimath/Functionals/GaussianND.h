//# GaussianND.h: A multidimensional Gaussian class
//# Copyright (C) 1995,1996,1998,1999,2001,2002,2004,2005
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
//# $Id$

#ifndef SCIMATH_GAUSSIANND_H
#define SCIMATH_GAUSSIANND_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/GaussianNDParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A Multi-dimensional Gaussian functional. </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tGaussianND" demos="dGaussianND">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="GaussianNDParam">GaussianNDParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <synopsis> 
// A <src>GaussianND</src> is used to calculate Gaussian functions of any
// dimension. A <linkto class=Gaussian1D> Gaussian1D </linkto> class exists
// which is more appropriate for one dimensional Gaussian functions, and a
// <linkto class=Gaussian2D> Gaussian2D </linkto> class exists for two
// dimensional functions.
//
// A statistical description of the multi-dimensional Gaussian is used (see
// Kendall & Stuart "The Advanced Theory of Statistics").  A Gaussian is
// defined in terms of its height, mean (which is the location of the peak
// value), variance, (a measure of the width of the Gaussian), and
// covariance which skews the distribution with respect to the Axes.
//
// In the general description the variance and covariance are specified
// using a covariance matrix.  This is defined as (for a 4 dimensional
// Gaussian):
// <srcblock>
//  V = |     s1*s1 r12*s1*s2 r13*s1*s3 r14*s1*s4 | 
//      | r12*s1*s2     s2*s2 r23*s2*s3 r24*s2*s4 |
//      | r13*s1*s3 r23*s2*s3     s3*s3 r34*s3*s4 |
//      | r14*s1*s4 r24*s2*s4 r34*s3*s4     s4*s4 |
// </srcblock>
// where s1 (<src>sigma1</src>) is the standard deviation of the Gaussian with
// respect to the first axis, and r12 (<src>rho12</src>) is the correlation
// between the the first and second axis. The correlation MUST be between -1
// and 1, and this class checks this as well as ensuring that the diagonal
// is positive. 
//
// <note role=warning> It is possible to have symmetric matrices that are of
// the above described form (ie. symmetric with <src>-1 <= rho(ij) <=1</src>)
// that do
// not generate a Gaussian function. This is because the Matrix is NOT
// positive definite (The limits on <src>rho(ij)</src> are upper limits).
// This class
// does check that the covariance Matrix is positive definite and will throw
// an exception (AipsError) if it is not.</note>
//
// The covariance Matrix can be specified by only its upper or lower
// triangular regions (ie. with zeros in the other triangle), otherwise it
// MUST be symmetric.
//
// The Gaussian that is constructed from this covariance Matrix (V), along
// with mean (u) and height (h) is:
// <srcblock>
//  f(x) = h*exp( -1/2 * (x-u) * V^(-1) * (x-u))
// </srcblock>
// where x, and u are vectors whose length is the dimensionality of the
// Gaussian and V^(-1) is the inverse of the covariance Matrix defined
// above. For a two dimensional Gaussian with zero mean this expression
// reduces to:
// <srcblock>
// f(x) = h*exp(-1/(2*(1-r12^2))*(x1^2/s1^2 - 2*r12*x1*x2/(s1*s2) + x2^2/s2^2))
// </srcblock>
//
// The amplitude of the Gaussian can be defined in two ways, either using
// the peak height (as is done in the constructors, and the setHeight
// function) or using the setFlux function. The flux in this context is the
// analytic integral of the Gaussian over all dimensions. Using the setFlux
// function does not modify the shape of the Gaussian just its height. 
//
// All the parameters of the Gaussian except its dimensionality can be
// modified using the set/get functions.
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting"> Fitting </linkto> classes. 
// There are always 4
// parameter sets. The parameters are, in order:
// <ol>
// <li> height (1 term). No assumptions on what quantity the height
//      represents, and it can be negative
// <li> mean (ndim terms).
// <li> variance (ndim terms). The variance is always positive, and an
//      exception (AipsError) will be thrown if you try to set a negative
//      value. 
// <li> covariance (ndim*(ndim-1)/2 terms) The order is (assuming ndim=5)
//      v12,v13,v14,v15,v23,v24,v25,v34,v35,v45. The restrictions described
//      above for the covariance (ie. -1 < r12 < +1) are enforced. 
// </ol>
// </synopsis> 

// <example>
// Construct a two dimensional Gaussian with mean=(0,1), variance=(.1,7) and
// height = 1;
// <srcblock>
// uInt ndim = 2;
// Float height = 1;
// Vector<Float> mean(ndim); mean(0) = 0, mean(1) = 1;
// Vector<Float> variance(ndim); variance(0) = .1, variance(1) = 7;
// GaussianND<Float> g(ndim, height, mean, variance); 
// Vector<Float> x(ndim); x = 0;
// cout << "g("<< x <<") = " << g(x) <<endl; // g([0,0])=1*exp(-1/2*1/7);
// x(1)++;
// cout << "g("<< x <<") = " <<g(x) <<endl;  // g([0,1])= 1
// cout << "Height: " << g.height() <<endl;    // Height: 1
// cout << "Flux: " << g.flux() << endl;       // Flux: 2*Pi*Sqrt(.1*7)
// cout << "Mean: " << g.mean() << endl;  // Mean: [0, -1]
// cout << "Variance: " << g.variance() <<endl;  // Variance: [.1, 7]
// cout << "Covariance: "<< g.covariance()<<endl;// Covariance: [.1, 0]
//                                                       //             [0,  7]
// g.setFlux(1);
// cout << "g("<< x <<") = " <<g(x) <<endl;  //g([0,1])=1/(2*Pi*Sqrt(.7))
// cout << "Height: " << g.height() <<endl;    // Height: 1/(2*Pi*Sqrt(.7))
// cout << "Flux: " << g.flux() << endl;       // Flux: 1
// cout << "Mean: " << g.mean() << endl;  // Mean: [0, -1]
// cout << "Variance: " << g.variance() <<endl;  // Variance: [.1, 7]
// cout << "Covariance: "<< g.covariance()<<endl;// Covariance: [.1, 0]
//                                                       //             [0,  7]
// </srcblock>
// </example>

// <motivation>
// A Gaussian Functional was needed for modeling the sky with a series of
// components. It was later realised that it was too general and Gaussian2D
// was written.  
// </motivation>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types.
// </templating>

// <todo asof="2001/08/19">
//  <li> Nothing I know off, apart from possible optimization
// </todo>

template<class T> class GaussianND : public GaussianNDParam<T>
{
public:
  //# Constructors
  // Makes a Gaussian using the indicated height, mean, variance &
  // covariance.
  // ndim defaults to 2, 
  // mean defaults to 0, 
  // height to  Pi^(-ndim/2) (the flux is unity)
  // variance defaults to 1.0, 
  // covariance defaults to 0.0, 
  // <group>
  GaussianND() : GaussianNDParam<T>() {}
  explicit GaussianND(uInt ndim) :
    GaussianNDParam<T>(ndim) {}
  GaussianND(uInt ndim, const T &height) :
    GaussianNDParam<T>(ndim, height) {}
  GaussianND(uInt ndim, const T &height, const Vector<T> &mean) :
    GaussianNDParam<T>(ndim, height, mean) {}
  GaussianND(uInt ndim, const T &height, const Vector<T> &mean,
	     const Vector<T> &variance) :
    GaussianNDParam<T>(ndim, height, mean, variance) {}
  GaussianND(uInt ndim, const T &height, const Vector<T> &mean,
	     const Matrix<T> &covar) :
    GaussianNDParam<T>(ndim, height, mean, covar) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  GaussianND(const GaussianND &other) : GaussianNDParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  GaussianND<T> &operator=(const GaussianND<T> &other) {
    GaussianNDParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~GaussianND() {}

  //# Operators    
  // Evaluate the Gaussian at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting this pointer.
  // <group>
  virtual Function<T> *clone() const { return new GaussianND<T>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using GaussianNDParam<T>::param_p;
  using GaussianNDParam<T>::itsDim;
public:
  using GaussianNDParam<T>::HEIGHT;
  using GaussianNDParam<T>::CENTER;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/GaussianND.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
