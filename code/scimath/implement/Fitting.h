//# Fitting.h: Module for various forms of mathematical fitting
//# Copyright (C) 1995,1999,2000
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
#if !defined(AIPS_FITTING_H)
#define AIPS_FITTING_H

#include <aips/aips.h>
#include <trial/Fitting/LSQBase.h>
#include <trial/Fitting/LinearFit.h>
#include <trial/Fitting/LinearFitSVD.h>
#include <trial/Fitting/NonLinearFit.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <trial/Fitting/LatticeFit.h>

// <module>
//
// <summary>
// Module for various forms of mathematical fitting. 
// </summary>
//
// <reviewed reviewer="" date="" demos="dLSQBase">
// </reviewed>
//
// <synopsis> 
//
// The Fitting module is intended to hold various classes and functions related
// to fitting models to data.  Currently classes in the module only handle
// least-squares fits. 
//
// <H3>Least-Squares Fits</H3>
//
// We are given N data points, which
// we will fit to a function with M adjustable parameters. 
// N should normally be greater than M, and at least M non-dependent relations
// between the parameters should be given. In cases where there are less than
// M independent points, Singular-Value-Deconvolution methods are available.
// Each condition equation can be given an  
// (estimated) standard deviation, which is comparable to the statistical
// weight, which is often used in place of the standard deviation.
// 
// The best fit is assumed to be the one which minimises the 'chi-squared'.
//
// In the (rather common) case that individual errors are not known for
// the individual data points, one can <em>assume</em> that the the
// individual errors are unity, calculate the best fit function, and then
// <em>estimate</em> the errors (assuming they are all identical) by
// inverting the <em>normal equation</em>.
// Of course, in this case we do not have an independent estimate of
// chi<sup>2</sup>.
//
// The methods used in the Fitting module are described in aips++ 
// <a href="../../notes/224/">Note 224</a>. The methods (both standard and
// SVD) are based on a Cholesky decomposition of the normal equations.
//
// General background can also be found in <EM>Numerical Recipes</EM> by 
// Press <EM>et al.</EM>.
//
// <H3>Linear Least-Squares Fits</H3>
//
// The <em>linear least squares</em> solution assumes that the fit function is a
// linear combination of M basis functions.
// It is important to note that <em>linear</em> refers to the dependence on
// the parameters; the basis functions themselves may be very non-linear.
//
// The linear least squares problem is solved by explicitly
// forming and inverting the normal equations.
// If the normal equations are close to
// singular, the <em>singular value decomposition</em> (SVD) method may be 
// used. <em>Numerical Recipes</em> suggests the SVD be always used, however 
// this advice is not universally accepted.
//
//
// <H3>Linear Least-Squares Fits with Known Linear Constraints</H3>
//
// Sometimes there are not enough independent observations, <EM>i.e.</EM>, the
// number of data points <I>N</I> is less than the number of adjustable
// parameters <I>M</I>.  In this case the least-squares problem cannot be
// solved unless additional ``constraints'' on the adjustable parameters can
// be introduced.  Under other circumstances, we may want to introduce
// constraints on the adjustable 
// parameters to add additional information, <EM>e.g.</EM>, the sum of angles 
// of a triangle.  In more complex cases, the forms of the constraints 
// are unknown.  Here we confine ourselves to
// least-squares fit problems in which the forms of constraints are known.
//
// If the forms of constraint equations are known, the least-squares
// problem can be solved.  (In the case where not 
// enough independent observations are available, a minimum number of 
// sufficient constraint equations have to be provided. The singular value
// decomposition method can be used to calculate the minimum number of 
// orthogonal constraints needed).
// 
// <H3>Nonlinear Least-Squares Fits</H3>
//
// We now consider the situation where the fitted function 
// depends <EM>nonlinearly</EM> on the set of
// <I>M</I> adjustable parameters.
// But with nonlinear dependences the minimisation of chi<sup>2</sup> cannot
// proceed as in the linear case.
// However, we can <EM>linearise</EM> the problem, find an
// approximate solution, and then iteratively seek the minimising solution. 
// The iteration stops when e.g. the adjusted parameters do not change
// anymore. In general it is very difficult to find a general solution that
// finds a global minimum, and the solution has to be matched with the problem.
// The Levenberg-Marquardt algorithm is a general non-linear fitting method
// which can produce correct results in many cases. It has been included, but
// always be aware of possible problems with non-linear solutions.
//
// <H3>What Is Available?</H3>
//
// Currently, the Fitting module has three different groups of classes:
// <ol>
// <li> Basic classes (<linkto class=LSQ>LSQ</linkto>, 
//  	<linkto class=LSQBasic>LSQBasic</linkto>,
// 	<linkto class=LSQBase>LSQBase</linkto>), which provide the basic 
// 	framework (create normal equations, solve, iterate) for real (LSQ),
// 	added with Complex (LSQBasic) added with aips++ array interface, and the
// 	normal class to be used (LSQBase). 
// <li> Fit a linear combination of functions to data points, and, optionally,
//      use supplied constraint conditions on the adjustable parameters.
// <li> Fit a nonlinear function to data points.  The adjustable parameters
//      are parameters inside the function.
// <li> Repetitively perform a linear fit for every line of pixels parallel 
//      to any axis in a Lattice.
// <li> Solve (as opposed to fit to a set of data), a set of equations
// </ol> 
//
// Note that the basic functions have <em>LSQ</em> in their title; the
// one-step fitting functions <em>Fit</em>, and the solution with more
// freedom <em>Solve</em>.
//
// The above fitting problems can usually be solved by directly 
// calling the fit(...) member function provided by the classes or by 
// gradually building the normal equation matrix and solving the
// normal equation (solve(...)).  
//  
// </synopsis> 
//
// <motivation>
// This module was motivated by baseline subtraction/continuum fitting in the
// first instance.
// </motivation>
//
// <todo asof="1999/11/12">
//	<li> extend the makeNorm() interface and the number of Fitting routines
// </todo>
//
// </module>
//
#endif


