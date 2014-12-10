//# Fitting.h: Module for various forms of mathematical fitting
//# Copyright (C) 1995,1999-2002,2004
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
#ifndef SCIMATH_FITTING_H
#define SCIMATH_FITTING_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Fitting/LSQFit.h>
#include <casacore/scimath/Fitting/LinearFit.h>
#include <casacore/scimath/Fitting/LinearFitSVD.h>
#include <casacore/scimath/Fitting/NonLinearFit.h>
#include <casacore/scimath/Fitting/NonLinearFitLM.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Module for various forms of mathematical fitting. 
// </summary>
//
// <prerequisite>
// <li> Basic principles can be found in 
//      <a href="../notes/224.html">Note 224</a>.
// </prerequisite>
//
// <reviewed reviewer="Neil Killeen" date="2000/06/01"
//	 demos="dLSQFit,dConstraints">
// </reviewed>
//
// <synopsis> 
//
// The Fitting module holds various classes and functions related
// to fitting models to data.  Currently only least-squares fits
// are handled.
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
// the individual data points, one can <em>assume</em> that the
// individual errors are unity, calculate the best fit function, and then
// <em>estimate</em> the errors (assuming they are all identical) by
// inverting the <em>normal equations</em>.
// Of course, in this case we do not have an independent estimate of
// chi<sup>2</sup>.
//
// The methods used in the Fitting module are described in 
// <a href="../notes/224.html">Note 224</a>.
// The methods (both standard and
// SVD) are based on a Cholesky decomposition of the normal equations.
//
// General background can also be found in <EM>Numerical Recipes</EM> by 
// Press <EM>et al.</EM>.
//
// <H3>Linear Least-Squares Fits</H3>
//
// The <em>linear least squares</em> solution assumes that the fit function
// is a linear combination of M linear condition equations.
// It is important to note that <em>linear</em> refers to the dependence on
// the parameters; the condition equations may be very non-linear in the
// dependent arguments.
//
// The linear least squares problem is solved by explicitly
// forming and inverting the normal equations.
// If the normal equations are close to
// singular, the <em>singular value decomposition</em> (SVD) method may be 
// used. <em>Numerical Recipes</em> suggests the SVD be always used, however 
// this advice is not universally accepted.
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
// The basic classes are <linkto class=LSQFit>LSQFit</linkto> and 
//  <linkto class=LSQaips>LSQaips</linkto>.
// They provide the basic  framework for normal equation generation, solving 
// the normal equations and iterating in the case of non-linear equations. 
//
// The <em>LSQFit</em> class uses a native C++ interface (pointers and 
// iterators). They handle real data and complex data.
// The <em>LSQaips</em> class offers the functionality of <em>LSQFit</em>,
// but with an additional Casacore Array interface.<br>
//
// Functionality is
// <ol>
// <li> Fit a linear combination of functions to data points, and, optionally,
//      use supplied constraint conditions on the adjustable parameters.
// <li> Fit a nonlinear function to data points.  The adjustable parameters
//      are parameters inside the function.
// <li> Repetitively perform a linear fit for every line of pixels parallel 
//      to any axis in a Lattice.
// <li> Solve (as opposed to fit to a set of data), a set of equations
// </ol> 
//
// In addition to the basic Least Squares routines in the <src>LSQFit</src> and
// <src>LSQaips</src> classes, this module contains also a set of direct
// data fitters:
// <ul>
// <li> <src>Fit2D</src>
// <li> <src>LinearFit</src>
// <li> <src>LinearFitSVD</src>
// <li> <src>NonLinearFit</src>
// <li> <src>NonLinearFitLM</src>
// </ul>
// Furthermore class <src>LatticeFit</src> can do fitting on lattices.
//
// Note that the basic functions have <em>LSQ</em> in their title; the
// one-step fitting functions <em>Fit</em>.
//
// The above fitting problems can usually be solved by directly 
// calling the <src>fit()</src> member function provided by one of the
// <src>Fit</src> classes above, or by 
// gradually building the normal equation matrix and solving the
// normal equations (<src>solve()</src>).  
//  
// A Distributed Object interface to the classes is available
// (<src>DOfitting</src>) for use in the <I>Glish</I> <src>dfit</src>
// object, available through the <src>fitting.g</src> script.
//
// </synopsis> 
//
// <motivation>
// This module was motivated by baseline subtraction/continuum fitting in the
// first instance.
// </motivation>
//
// <todo asof="2004/09/22">
//	<li> extend the Fit interface classes to cater for building the
//		normal equations in parts.
// </todo>
//
// </module>
//

} //# NAMESPACE CASACORE - END

#endif


