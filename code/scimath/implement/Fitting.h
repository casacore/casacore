//# Fitting.h: Module for various forms of mathematical fitting
//# Copyright (C) 1995
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
#include <trial/Fitting/LeastSquares.h>
#include <trial/Fitting/LinearFit.h>
#include <trial/Fitting/LinearFitSVD.h>
#include <trial/Fitting/LinearFitConstraint.h>
#include <trial/Fitting/LinearFitCnstrntLU.h>
#include <trial/Fitting/NonLinearFit.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <trial/Fitting/LSQMethods.h>
#include <trial/Fitting/LatticeFit.h>

// <module>
//
// <summary>
// Module for various forms of mathematical fitting. 
// </summary>
//
// <reviewed reviewer="" date="" demos="">
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
// This discussion generally follows the description in in <em>Numerical
// Recipes</em> by Press <em>et al</em>. This section is mainly intended to
// unambiguously convey the definitions used by these classes, not to
// develop the subject in any depth.
//
// We are given N data points 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap54.gif"> which
// we will fit to a function with M adjustable parameters 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap56.gif">. 
// Of course N should normally be greater than M.  Here, 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap58.gif"> 
// are the (estimated) standard deviations of the 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap60.gif">;
// i.e. they share the same units. Although it is not used here, the
// statistical weight (<IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap62.gif">) 
// is often used instead of the standard deviation.
// 
// The best fit is assumed to be the one which minimizes:<P>
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath50.gif"><P>
// That is, the parameters <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap64.gif">
// are adjusted to minimize <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap66.gif">.
// For large N - M and normally distributed data values, a reasonably good
// fit has <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap68.gif">.
//
// In the (rather common) case that individual errors are not known for
// the individual data points, one can <em>assume</em> that the the
// individual errors are unity, calculate the best fit function, and then
// <em>estimate</em> the errors (assuming they are all identical) by
// inverting the above:<P><IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath51.gif"><P>
// Of course in this case we do not have an independent estimate of
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap66.gif">! (The denominator is 
// N-M rather than N to make it an unbiased estimate).
//
// <H3>Linear Least-Squares Fits</H3>
//
// The discussion so far has not assumed anything about the
// functional dependence of the fit function on its parameters, i.e. the
// discussion has been for <em>general least squares</em> fitting. The 
// <em>linear least squares</em> solution assumes that the fit function is a
// linear combination of M basis functions, <em>i.e.</em>:<P>
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath72.gif"><P>
// It is important to note that <em>linear</em> refers to the dependence on
// the parameters; the basis functions themselves may be very non-linear.
//
// The <em>Design Matrix</em> is defined to be:<P>
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath73.gif"><P>
// Also define a normalized data vector 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap82.gif">.
//
// With these definitions, the linear least squares problem can be
// formulated in two (equivalent) ways; by either minimizing:<P>
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath74.gif"><P>
// or by solving the matrix equations (the so called <em>normal equations</em>):
// <P><IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath75.gif"><P>
// The <em>covariance matrix</em> is defined as
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap84.gif">. Using this definition
// and the above, we see that the equation for the parameters may be written as:
// <P><IMG ALIGN=BOTTOM SRC="Fitting/Fitting_displaymath76.gif"><P>
// The covariance matrix gives the estimated variances of the fitted
// parameters. Note in particular that 
// <IMG ALIGN=BOTTOM SRC="Fitting/Fitting_tex2html_wrap86.gif">, <em>i.e.</em>,
// the estimated standard deviations of the fitted parameters is
// the square root of the diagonal elements of the covariance matrix.
//
// The linear least squares problem is usually solved by explicitly
// forming and inverting the normal equations or by supplying a standard
// linear algebra matrix equation solver (such as <em>LU</em> or <em>QR
// decomposition</em>).  If the normal equations are close to
// singular, the <em>singular value decomposition</em> (SVD) method may be 
// used. <em>Numerical Recipes</em> suggests the SVD be always used, however 
// this advice is not universally accepted.
//
//
//<H3>Linear Least-Squares Fits with Known Linear Constraints</A></H3>
//<P>
//The following discussion generally follows the description in 
//<EM>Least-Squares in New Star</EM> By W. Brouw.
//<P>
//Sometimes there are not enough independent observations, <EM>i.e.</EM>, the 
//number of data points <I>N</I> is less than the number of adjustable parameters 
//<I>M</I>.  In this case the least-squares problem cannot be solved unless additional
//``constraints'' on the adjustable parameters can be introduced.  Under
//other circumstances, we may want to introduce constraints on the adjustable 
//parameters to add additional information, <EM>e.g.</EM>, the sum of angles
//of a triangle.  In more complex cases, the forms of the constraints
//are unknown.  Here we confine ourselves to least-squares fit problems in which
//the forms of constraints are known.
//<P>
//If the forms of constraint equations are known, the least-squares
//problem can be solved.  (In the case where not 
//enough independent observations are available, a minimum number of 
//sufficient constraint equations have to be provided. The singular value
//decomposition method can be used to calculate the minimum number of 
//orthogonal constraints needed).
//Suppose we have a set of <I>P</I> linear constraint 
//equations for the <I>M</I> adjustable parameters
//<IMG WIDTH=141 HEIGHT=27 ALIGN=MIDDLE ALT="tex2html_wrap_inline189" SRC="Fitting/Fitting_math_img1.gif">:
//<BR><IMG WIDTH=402 HEIGHT=17 ALIGN=BOTTOM ALT="displaymath171" SRC="Fitting/Fitting_math_img2.gif"><BR>
//<BR><IMG WIDTH=294 HEIGHT=16 ALIGN=BOTTOM ALT="displaymath172" SRC="Fitting/Fitting_math_img3.gif"><BR>
//where <IMG WIDTH=25 HEIGHT=24 ALIGN=MIDDLE ALT="tex2html_wrap_inline191" SRC="Fitting/Fitting_math_img4.gif"> and <IMG WIDTH=12 HEIGHT=16 ALIGN=MIDDLE ALT="tex2html_wrap_inline193" SRC="Fitting/Fitting_math_img5.gif"> are known constants. The design matrix and the
//normalized data vector are defined as
//<BR><IMG WIDTH=304 HEIGHT=39 ALIGN=BOTTOM ALT="displaymath173" SRC="Fitting/Fitting_math_img6.gif"><BR>
//<BR><IMG WIDTH=279 HEIGHT=34 ALIGN=BOTTOM ALT="displaymath174" SRC="Fitting/Fitting_math_img7.gif"><BR>
//the matrix equation to be solved then becomes
//<BR><IMG WIDTH=359 HEIGHT=50 ALIGN=BOTTOM ALT="displaymath175" SRC="Fitting/Fitting_math_img8.gif"><BR>
//where <IMG WIDTH=140 HEIGHT=27 ALIGN=MIDDLE ALT="tex2html_wrap_inline195" SRC="Fitting/Fitting_math_img9.gif"> are
//Lagrange multiplicators which are solved simultaneously with <IMG WIDTH=9 HEIGHT=8 ALIGN=BOTTOM ALT="tex2html_wrap_inline197" SRC="Fitting/Fitting_math_img10.gif">. 
//A number of standard methods, such as LU and QR decomposition, can be applied 
//to solve this matrix equation.
//<P>
//
//<H3>Nonlinear Least-Squares Fits</A></H3>
//<P>
//For more rigorous treatment on this topic see <EM>Numerical Recipes</EM> by 
//Press <EM>et al</EM>.
//<P>
//We now consider the situation where the fitted function 
//<IMG WIDTH=115 HEIGHT=27 ALIGN=MIDDLE ALT="tex2html_wrap_inline215" SRC="Fitting/Fitting_math_img11.gif"> depends <EM>nonlinearly</EM> on the set of
//<I>M</I> adjustable parameters, <IMG WIDTH=141 HEIGHT=27 ALIGN=MIDDLE ALT="tex2html_wrap_inline189" SRC="Fitting/Fitting_math_img1.gif">. 
//We construct <IMG WIDTH=17 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="Fitting/Fitting_math_img12.gif"> as before:
//<BR><IMG WIDTH=380 HEIGHT=49 ALIGN=BOTTOM ALT="displaymath199" SRC="Fitting/Fitting_math_img13.gif"><BR>
//But with nonlinear dependences the minimization cannot proceed as in
//linear fits.  However, we can <EM>linearize</EM> the problem, find an approximate
//solution, and then iteratively seek the minimizing solution.  The iteration 
//stops when <IMG WIDTH=17 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="Fitting/Fitting_math_img12.gif"> stops decreasing.  We expand the fitted function at
//an initially guessed solution for the adjustable parameters, 
//<IMG WIDTH=152 HEIGHT=34 ALIGN=MIDDLE ALT="tex2html_wrap_inline225" SRC="Fitting/Fitting_math_img14.gif">, 
//<BR><IMG WIDTH=477 HEIGHT=52 ALIGN=BOTTOM ALT="displaymath200" SRC="Fitting/Fitting_math_img15.gif"><BR>
//where <IMG WIDTH=112 HEIGHT=35 ALIGN=MIDDLE ALT="tex2html_wrap_inline227" SRC="Fitting/Fitting_math_img16.gif">.
//Substituting this equation into the <IMG WIDTH=17 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="Fitting/Fitting_math_img12.gif"> expression and ignoring the 
//higher order terms, <IMG WIDTH=67 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline231" SRC="Fitting/Fitting_math_img17.gif">, we find
//<BR><IMG WIDTH=490 HEIGHT=62 ALIGN=BOTTOM ALT="displaymath201" SRC="Fitting/Fitting_math_img18.gif"><BR>
//This closely resembles the problem that linear least-squares fit solves.
//In fact <IMG WIDTH=52 HEIGHT=30 ALIGN=MIDDLE ALT="tex2html_wrap_inline233" SRC="Fitting/Fitting_math_img19.gif"> differs from <IMG WIDTH=17 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="Fitting/Fitting_math_img12.gif"> only by 
//<IMG WIDTH=60 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline237" SRC="Fitting/Fitting_math_img20.gif"> which is small.  If we define the design matrix
//<BR><IMG WIDTH=315 HEIGHT=50 ALIGN=BOTTOM ALT="displaymath202" SRC="Fitting/Fitting_math_img21.gif"><BR>
//and the normalized data vector
//<BR><IMG WIDTH=326 HEIGHT=41 ALIGN=BOTTOM ALT="displaymath203" SRC="Fitting/Fitting_math_img22.gif"><BR>
//we recover the linear least-squares matrix equation
//<BR><IMG WIDTH=343 HEIGHT=23 ALIGN=BOTTOM ALT="displaymath204" SRC="Fitting/Fitting_math_img23.gif"><BR>
//We can solve for <IMG WIDTH=19 HEIGHT=13 ALIGN=BOTTOM ALT="tex2html_wrap_inline239" SRC="Fitting/Fitting_math_img24.gif"> and use it to form an improved solution
//<BR><IMG WIDTH=309 HEIGHT=25 ALIGN=BOTTOM ALT="displaymath205" SRC="Fitting/Fitting_math_img25.gif"><BR>
//<BR><IMG WIDTH=297 HEIGHT=16 ALIGN=BOTTOM ALT="displaymath206" SRC="Fitting/Fitting_math_img26.gif"><BR>
//Repeating the above procedure we may eventually reach the solution that
//minimizes <IMG WIDTH=17 HEIGHT=31 ALIGN=MIDDLE ALT="tex2html_wrap_inline221" SRC="Fitting/Fitting_math_img12.gif">.
//
// <H3>What Is Available?</H3>
//
// Currently, the following fitting can be performed using classes and
// function in the Fitting module: 
// <ol>
// <li> Fit a linear combination of functions to data points.  The adjustable
//      parameters are the function multiplicators in the linear combination.
//      Parameters inside the functions cannot be adjusted and thus fitted.  
//      The linear combination should be produced using the 
//      <linkto class="LinearComb">LinearComb</linkto> class.
// <li> Fit a linear combination of functions to data points use
//      supplied constraint conditions on the adjustable parameters.  The
//      constraint equations should be produced using the 
//      <linkto class="HyperPlane">HyperPlane</linkto> class.
// <li> Fit a nonlinear function to data points.  The adjustable parameters
//      are parameters inside the function. A combination of nonlinear
//      functions can be produced using the 
//      <linkto class="SumFunction">SumFunction</linkto> class.
// <li> Repetitively perform a linear fit for every line of pixels parallel 
//      to any axis in a Lattice.
// </ol> 
//
// The above fitting problems can usually be solved by directly 
// calling the fit(...) member function provided by the classes or by 
// gradually building the normal equation matrix and solving the
// normal equation.  
//  
// More detailed information and examples on these fits are available
// in <linkto class="LeastSquares">LeastSquares</linkto> class header
// file.   
// 
// <H3>List of Classes</H3>
//
// The module includes the following abstract classes:
// <ol>
// <li> LeastSquares, an abstract base class for all fitting classes using the
//            least-squares method.
// <li> LinearFit, an abstract base class for all fitting classes that fit a
//            linear combination of functions to data points.  This class
//            inherits LeastSquares.
//            <note role=tip>No provision has been made for the situation
//            in which both a complex variable and its complex conjugate 
//            appear as the adjustable parameters.  Such situation is 
//            currently handled as a linear fit with linear constraints 
//            problem.
//            </note> 
// <li> NonLinearFit, an abstract base class for all fitting classes that fit a
//            nonlinear function to data points. This class inherits 
//            LeastSquares
// <li> LinearFitConstraint, an abstract base class for all classes that 
//            perform linear fitting with
//            linear constraints.  This class inherits LinearFit.
// </ol>
// It also includes the following user callable classes and functions:
// <ol>
// <li> LinearFitSVD, a class that implements the singular value decomposition
//            fitting method to fit a linear combination of functions to data 
//            points. This class inherits LinearFit.
// <li> LinearFitConstraintLU, a class that implements the LU factorization 
//            method to solve the linear fit with linear constraints problems.
//            If constraints are not present, this class solves the same 
//            problem that LinearFitSVD solves using a different solution 
//            method. This class inherits LinearFitConstraint.
// <li> NonLinearFitLM, a class that implements the Levenberg-Marquardt method 
//            to solve nonlinear fitting problems.  The matrix equation is 
//            solved by the LU factorization method. This class inherits
//            NonLinearFit.
// <li> LSQMethods, a class consists of static functions to perform singular 
//            value decomposition, LU factorization, generation of normal 
//            equations, converting statistic weight to standard deviation,
//            and setting zero matrix diagonal elements to one.
//            It also includes interfaces to FORTRAN subroutines in LAPACK.  
//            This is a stand-alone class.
// <li> baselineFit, a function which repetitively does a linear fit for every 
//            line of pixels parallel to any axis in a Lattice. A mask
//            may be specified, and either the model or the residuals may be 
//            returned.
// </ol>
//
// <H3>Class Inheritance Hierarchy</H3>
//
// The class inheritance hierarchy for least-squares fits is as follows:
// <srcblock>
// LeastSquares 
//    |
//    |---LinearFit
//    |      |
//    |      |---LinearFitConstraint
//    |      |       |
//    |      |       |--LinearFitConstraintLU 
//    |      |
//    |      |---LinearFitSVD
//    |
//    |---NonLinearFit
//             |
//             |---NonLinearFitLM
//       
// </srcblock>
//
// </synopsis> 
//
// <motivation>
// This module was motivated by baseline subtraction/continuum fitting.
// </motivation>
//
// <todo asof="1996/11/12">
// <li> Handle cases in which a complex variable and its complex 
//      conjugate appear as the adjustable parameters.  Though Such cases are
//      currently handled as a linear fit with linear constraints problem,
//      it would save computation time if they are handled as
//      a linear fit without constraint problem.
// </todo>
// </module>
//
#endif


