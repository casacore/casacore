//# extern_fft.h: C++ wrapper functions for FORTRAN FFT code
//# Copyright (C) 1993,1994,1995,1997,1999,2000
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

#ifndef SCIMATH_SCSL_H
#define SCIMATH_SCSL_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>C++ Interface to the Sgi/Cray Scientific Library (SCSL)</summary>
// <synopsis>
// These are C++ wrapper functions for the transform routines in the SGI/Cray 
// Scientific Library (SCSL). The purpose of these definitions is to overload
// the functions so that C++ users can access the functions in SCSL with
// identical function names.
//
// <note role=warning> 
// Currently, the SCSL is available only on SGI machines.
// </note>
// </synopsis>

class SCSL
{
public:
// These routines compute the Fast Fourier Transform (FFT) of the complex
// vector x, and store the result in vector y.  <src>ccfft</src> does the
// complex-to-complex transform and <src>zzfft</src> does the same for double
// precision arrays.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  Suppose that the arrays are
// dimensioned as follows:
//
// <srcblock>
//      COMPLEX X(0:N-1), Y(0:N-1)
// </srcblock>
//
// The output array is the FFT of the input array, using the following
// formula for the FFT:
//
// <srcblock>
//                     n-1
//      Y(k) = scale * Sum [ X(j)*w**(isign*j*k) ]    for k = 0, ..., n-1
//                     j=0
//
//      where:
//      w = exp(2*pi*i/n),
//      i = + sqrt(-1),
//      pi = 3.14159...,
//      isign = +1 or -1
// </srcblock>
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make this routine compute any of the various possible definitions,
// however, by choosing the appropriate values for isign and scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and 1/(n*scale).
// In particular, if you use isign = +1 and scale = 1.0 you can compute
// the inverse FFT by using isign = -1 and scale = 1.0/n.
//
// The output array may be the same as the input array.
//
// <h3>Initialization</h3>
// The table array stores the trigonometric tables used in calculation of
// the FFT.  You must initialize table by calling the routine with isign
// = 0 prior to doing the transforms.  If the value of the problem size,
// n, does not change, table does not have to be reinitialized.
//
// <h3>Dimensions</h3>
//  In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared as follows:
//
// <srcblock>
//      COMPLEX X(0:N-1)
//      COMPLEX Y(0:N-1)
// </srcblock>
//
// However, if you prefer to use the more customary FORTRAN style with
// subscripts starting at 1 you do not have to change the calling
// sequence, as in the following (assuming N > 0):
//
// <srcblock>
//      COMPLEX X(N)
//      COMPLEX Y(N)
// </srcblock>
//
// <example>
// These examples use the table and workspace sizes appropriate to the
// Origin series.
//
// Example 1:  Initialize the complex array table in preparation for
// doing an FFT of size 1024.  Only the isign, n, and table arguments are
// used in this case.  You can use dummy arguments or zeros for the other
// arguments in the subroutine call.
//
// <srcblock> 
//      REAL TABLE(30 + 2048)
//      CALL CCFFT(0, 1024, 0.0, DUMMY, DUMMY, TABLE, DUMMY, 0)
// </srcblock> 
//
// Example 2:  x and y are complex arrays of dimension (0:1023).  Take
// the FFT of x and store the results in y.  Before taking the FFT,
// initialize the table array, as in example 1.
//
// <srcblock> 
//      COMPLEX X(0:1023), Y(0:1023)
//      REAL TABLE(30 + 2048)
//      REAL WORK(2048)
//      CALL CCFFT(0, 1024, 1.0, X, Y, TABLE, WORK, 0)
//      CALL CCFFT(1, 1024, 1.0, X, Y, TABLE, WORK, 0)
// </srcblock> 
//
// Example 3:  Using the same x and y as in example 2, take the inverse
// FFT of y and store it back in x.  The scale factor 1/1024 is used.
// Assume that the table array is already initialized.
//
// <srcblock> 
//      CALL CCFFT(-1, 1024, 1.0/1024.0, Y, X, TABLE, WORK, 0)
// </srcblock> 
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  No change was
// needed in the subroutine calls.
//
// <srcblock> 
//      COMPLEX X(1024), Y(1024)
//      CALL CCFFT(0, 1024, 1.0, X, Y, TABLE, WORK, 0)
//      CALL CCFFT(1, 1024, 1.0, X, Y, TABLE, WORK, 0)
// </srcblock> 
//
// Example 5:  Do the same computation as in example 4, but put the
// output back in array x to save storage space.  Assume that table is
// already initialized.
//
// <srcblock> 
//      COMPLEX X(1024)
//      CALL CCFFT(1, 1024, 1.0, X, X, TABLE, WORK, 0)
// </srcblock> 
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n</b>
// <dd>    Integer.  Size of the transform (the number of values in
//         the input array).  n >= 1.
// <dt><b>scale</b>
// <dd>    Scale factor.  
//         <src>ccfft</src>: real.
//         <src>zzfft</src>: double precision.
//         Each element of the output array is multiplied by scale
//         after taking the Fourier transform, as defined by the previous
//         formula.
// <dt><b>x</b>
// <dd>    Array of dimension (0:n-1).
//         <src>ccfft</src>: complex array.
//         <src>zzfft</src>: double complex array. 
// 
//         Input array of values to be transformed.
// <dt><b>isys</b>
// <dd>    Integer.
//         Algorithm used; value dependent on hardware system.  Currently, no
//         special options are supported; therefore, you must always specify
//         an isys argument as constant 0.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Array of dimension (0:n-1).
//         <src>ccfft</src>: complex array.
//         <src>zzfft</src>: double complex array.
//         Output array of transformed values.  The output array may be
//         the same as the input array.  In that case, the transform is
//         done in place and the input array is overwritten with the
//         transformed values.
// <dt><b>table</b>
// <dd>    Real array; dimension 2*n+30.
//
//         Table of factors and trigonometric functions.
//
//         If isign = 0, the routine initializes table (table is output
//         only).
//
//         If isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0 (table is
//         input only).
// <dt><b>work</b>
// <dd>    Real array; dimension 2*n.
//
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different address
//         space from that of the input and output arrays.
// </dl>
// <group>
static void ccfft(Int isign, Int n, Float scale, Complex* x,
		  Complex* y, Float* table, Float* work, Int isys);
static void ccfft(Int isign, Int n, Double scale, DComplex* x,
		  DComplex* y, Double* table, Double* work, Int isys);
static void zzfft(Int isign, Int n, Double scale, DComplex* x,
		  DComplex* y, Double* table, Double* work, Int isys);
// </group>

// <src>scfft/dzfft</src> computes the FFT of the real array x, and it stores
// the results in the complex array y.  <src>csfft/zdfft</src> computes the
// corresponding inverse complex-to-real transform.
//
// It is customary in FFT applications to use zero-based subscripts; the
// formulas are simpler that way.  For these routines, suppose that the
// arrays are dimensioned as follows:
//
// <srcblock>
//      REAL    X(0:n-1)
//      COMPLEX Y(0:n/2)
// </srcblock>
//
// Then the output array is the FFT of the input array, using the
// following formula for the FFT:
//
// <srcblock> 
//                     n-1
//      Y(k) = scale * Sum [ X(j)*w**(isign*j*k) ]    for k = 0, ..., n/2
//                     j=0
//
//      where:
//      w = exp(2*pi*i/n),
//      i = + sqrt(-1),
//      pi = 3.14159...,
//      isign = +1 or -1.
// </srcblock> 
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make these routines compute any of the various possible
// definitions, however, by choosing the appropriate values for isign and
// scale.
//
// The relevant fact from FFT theory is this:  If you call <src>scfft</src> 
// with any particular values of isign and scale, the mathematical inverse
// function is computed by calling <src>csfft</src> with -isign and 
// 1/(n*scale).  In particular, if you use isign = +1 and scale = 1.0 in 
// <src>scfft</src> for the forward FFT, you can compute the inverse FFT by
// using <src>ccfft</src> with isign = -1 and scale = 1.0/n.
//
// <h3>Real-to-complex FFTs</h3>
// Notice in the preceding formula that there are n real input values,
// and n/2 + 1 complex output values.  This property is characteristic of
// real-to-complex FFTs.
//
// The mathematical definition of the Fourier transform takes a sequence
// of n complex values and transforms it to another sequence of n complex
// values.  A complex-to-complex FFT routine, such as <src>ccfft</src>, will
// take n complex input values, and produce n complex output values.  In
// fact, one easy way to compute a real-to-complex FFT is to store the
// input data in a complex array, then call routine <src>ccfft</src> to
// compute the FFT.  You get the same answer when using the <src>scfft</src>
// routine.
//
// The reason for having a separate real-to-complex FFT routine is
// efficiency.  Because the input data is real, you can make use of this
// fact to save almost half of the computational work.  The theory of
// Fourier transforms tells us that for real input data, you have to
// compute only the first n/2 + 1 complex output values, because the
// remaining values can be computed from the first half of the values by
// the simple formula:
//
// <srcblock> 
//      Y(k) = conjg(Y(n-k)) for n/2 <= k <= n-1
// </srcblock>
//
// where the notation conjgY represents the complex conjugate of y.
//
// In fact, in many applications, the second half of the complex output
// data is never explicitly computed or stored.  Likewise, as explained
// later, only the first half of the complex data has to be supplied for
// the complex-to-real FFT.
//
// Another implication of FFT theory is that, for real input data, the
// first output value, Y(0), will always be a real number; therefore, the
// imaginary part will always be 0.  If n is an even number, Y(n/2) will
// also be real and thus, have zero imaginary parts.
//
// <h3>Complex-to-real FFTs</h3>
// Consider the complex-to-real case.  The effect of the computation is
// given by the preceding formula, but with X complex and Y real.
//
// Generally, the FFT transforms a complex sequence into a complex
// sequence.  However, in a certain application we may know the output
// sequence is real.  Often, this is the case because the complex input
// sequence was the transform of a real sequence.  In this case, you can
// save about half of the computational work.
//
// According to the theory of Fourier transforms, for the output
// sequence, Y, to be a real sequence, the following identity on the
// input sequence, X, must be true:
//
// <srcblock> 
//      X(k) = conjg(X(n-k)) for n/2 <= k <= n-1
// </srcblock> 
//
// And, in fact, the input values X(k) for k > n/2 need not be supplied;
// they can be inferred from the first half of the input.
//
// Thus, in the complex-to-real routine, <src>csfft</src>, the arrays can be
// dimensioned as follows:
//
// <srcblock> 
//      COMPLEX X(0:n/2)
//      REAL    Y(0:n-1)
// </srcblock> 
//
// There are n/2 + 1 complex input values and n real output values.  Even
// though only n/2 + 1 input values are supplied, the size of the
// transform is still n in this case, because implicitly you are using
// the FFT formula for a sequence of length n.
//
// Another implication of the theory is that X(0) must be a real number
// (that is, it must have zero imaginary part).  Also, if n is even,
// X(n/2) must also be real.  Routine <src>CSFFT</src> assumes that these
// values are real; if you specify a nonzero imaginary part, it is ignored.
//
// <h3>Table Initialization</h3>
// The table array stores the trigonometric tables used in calculation of
// the FFT.  This table must be initialized by calling the routine with
// isign = 0 prior to doing the transforms.  The table does not have to
// be reinitialized if the value of the problem size, n, does not change.
// Because <src>scfft</src> and <src>csfft</src> use the same format for 
// table, either can be used to initialize it (note that CCFFT uses a
// different table format).
//
// <h3>Dimensions</h3>
// In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared (assuming n > 0):
//
// <srcblock> 
//      REAL    X(0:n-1)
//      COMPLEX Y(0:n/2)
// </srcblock> 
//
// No change is needed in the calling sequence; however, if you prefer
// you can use the more customary Fortran style with subscripts starting
// at 1, as in the following:
//
// <srcblock> 
//      REAL    X(n)
//      COMPLEX Y(n/2 + 1)
// </srcblock> 
//
// <example>
// These examples use the table and workspace sizes appropriate to Origin
// series.
//
// Example 1:  Initialize the complex array TABLE in preparation for
// doing an FFT of size 1024.  In this case only the arguments isign, n,
// and table are used. You can use dummy arguments or zeros for the other
// arguments in the subroutine call.
//
// <srcblock> 
//      REAL TABLE(15 + 1024)
//      CALL SCFFT(0, 1024, 0.0, DUMMY, DUMMY, TABLE, DUMMY, 0)
// </srcblock> 
//
// Example 2:  X is a real array of dimension (0:1023), and Y is a
// complex array of dimension (0:512).  Take the FFT of X and store the
// results in Y.  Before taking the FFT, initialize the TABLE array, as
// in example 1.
//
// <srcblock> 
//      REAL X(0:1023)
//      COMPLEX Y(0:512)
//      REAL TABLE(15 + 1024)
//      REAL WORK(1024)
//      CALL SCFFT(0, 1024, 1.0, X, Y, TABLE, WORK, 0)
//      CALL SCFFT(1, 1024, 1.0, X, Y, TABLE, WORK, 0)
// </srcblock> 
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/1024 is used.  Assume that
// the TABLE array is initialized already.
//
// <srcblock> 
//      CALL CSFFT(-1, 1024, 1.0/1024.0, Y, X, TABLE, WORK, 0)
// </srcblock> 
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  The
// subroutine calls are not changed.
//
// <srcblock> 
//      REAL X(1024)
//      COMPLEX Y(513)
//      CALL SCFFT(0, 1024, 1.0, X, Y, TABLE, WORK, 0)
//      CALL SCFFT(1, 1024, 1.0, X, Y, TABLE, WORK, 0)
// </srcblock> 
//
// Example 5:  Perform the same computation as in example 4, but
// equivalence the input and output arrays to save storage space.  Assume
// that the TABLE array is initialized already.
//
// <srcblock> 
//      REAL X(1024)
//      COMPLEX Y(513)
//      EQUIVALENCE ( X(1), Y(1) )
//      CALL SCFFT(1, 1024, 1.0, X, Y, TABLE, WORK, 0)
// </srcblock> 
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n</b>
// <dd>    Integer.
//         Size of transform.  If n <= 2, <src>scfft/dzfft</src>
//         returns without calculating the transform.
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>scfft</src>: real. 
//         <src>dzfft</src>: double precision.
//         <src>csfft</src>: real.
//         <src>zdfft</src>: double precision.
//         Each element of the output array is multiplied by scale
//         after taking the Fourier transform, as defined by the previous
//         formula.
// <dt><b>x</b>
// <dd>    Input array of values to be transformed.
//         <src>scfft</src>: real array of dimension (0:n-1).
//         <src>dzfft</src>: double precision array of dimension (0:n-1).
//         <src>csfft</src>: complex array of dimension (0:n/2).
//         <src>zdfft</src>: double complex array of dimension (0:n/2).
// <dt><b>isys</b>
// <dd>    Integer array of dimension (0:isys(0)).
//         Use isys to specify certain processor-specific parameters or
//         options.  The first element of the array specifies how many
//         more elements are in the array.
//
//         If isys(0) = 0, the default values of such parameters are
//         used.  In this case, you can specify the argument value as
//         the scalar integer constant 0.  If isys(0) > 0, isys(0)
//         gives the upper bound of the isys array; that is, if
//         il = isys(0), user-specified parameters are expected in
//         isys(1) through isys(il).
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Output array of transformed values.
//         <src>scfft</src>: complex array of dimension (0:n/2).
//         <src>dzfft</src>: double complex array of dimension (0:n/2).
//         <src>csfft</src>: real array of dimension (0:n-1).
//         <src>zdfft</src>: double precision array of dimension (0:n-1).
//
//         The output array, y, is the FFT of the the input array, x,
//         computed according to the preceding formula.  The output
//         array may be equivalenced to the input array in the calling
//         program.  Be careful when dimensioning the arrays, in this
//         case, to allow for the fact that the complex array contains
//         two (real) words more than the real array.
// <dt><b>table</b>
// <dd>    Real array; dimension n+15.
//
//         Table of factors and trigonometric functions.
//
//         If isign = 0, the table array is initialized to contain
//         trigonometric tables needed to compute an FFT of size n.
//
//         If isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0.
// <dt><b>work</b>
// <dd>    Real array; dimension n.
//
//         Work array used for intermediate calculations.  Its address
//         space must be different from that of the input and output
//         arrays.
// </dl>
// <group>
static void scfft(Int isign, Int n, Float scale, Float* x,
		  Complex* y, Float* table, Float* work, Int isys); 
static void scfft(Int isign, Int n, Double scale, Double* x,
		  DComplex* y, Double* table, Double* work, Int isys);
static void dzfft(Int isign, Int n, Double scale, Double* x,
		  DComplex* y, Double* table, Double* work, Int isys);
static void csfft(Int isign, Int n, Float scale, Complex* x,
		  Float* y, Float* table, Float* work, Int isys); 
static void csfft(Int isign, Int n, Double scale, DComplex* x, 
		  Double* y, Double* table, Double* work, Int isys);
static void zdfft(Int isign, Int n, Double scale, DComplex* x, 
		  Double* y, Double* table, Double* work, Int isys);
// </group>

// <src>ccfftm/zzfftm</src> computes the FFT of each column of the
// complex matrix x, and stores the results in the columns of complex
// matrix y.
//
// Suppose the arrays are dimensioned as follows:
//
// <srcblock>
//      COMPLEX X(0:ldx-1, 0:lot-1)
//      COMPLEX Y(0:ldy-1, 0:lot-1)
//
// where ldx >= n, ldy >= n.
// </srcblock>
//
// Then column L of the output array is the FFT of column L of the
// input array, using the following formula for the FFT:
//
// <srcblock>
//                        n-1
//      Y(k, L) = scale * Sum [ X(j)*w**(isign*j*k) ]
//                        j=0
//      for k = 0, ..., n-1
//          L = 0, ..., lot-1
//      where:
//          w = exp(2*pi*i/n),
//          i = + sqrt(-1),
//          pi = 3.14159...,
//          isign = +1 or -1
//          lot = the number of columns to transform
// </srcblock>
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make this routine compute any of the various possible definitions,
// however, by choosing the appropriate values for isign and scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and 1/(n * scale).
// In particular, if you use isign = +1 and scale = 1.0 for the forward
// FFT, you can compute the inverse FFT by using the following:  isign =
// -1 and scale = 1.0/n.
//
// This section contains information about the algorithm for these
// routines, the initialization of the table array, the declaration of
// dimensions for x and y arrays, some performance tips, and some
// implementation-dependent details.
//
// <h3>Algorithm</h3>
// These routines use decimation-in-frequency type FFT.  It takes the FFT
// of the columns and vectorizes the operations along the rows of the
// matrix.  Thus, the vector length in the calculations depends on the
// row size, and the strides for vector loads and stores are the leading
// dimensions, ldx and ldy.
//
// <h3>Initialization</h3>
// The table array stores the trigonometric tables used in calculation of
// the FFT.  You must initialize the table array by calling the routine
// with isign = 0 prior to doing the transforms.  If the value of the
// problem size, n, does not change, table does not have to be
// reinitialized.
//
// <h3>Dimensions</h3>
// In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared as follows:
//
// <srcblock> 
//      COMPLEX X(0:ldx-1, 0:lot-1)
//      COMPLEX Y(0:ldy-1, 0:lot-1)
// </srcblock> 
//
// The calling sequence does not have to change, however, if you prefer
// to use the more customary Fortran style with subscripts starting at 1.
// The same values of ldx and ldy would be passed to the subroutine even
// if the input and output arrays were dimensioned as follows:
//
// <srcblock> 
//      COMPLEX X(ldx, lot)
//      COMPLEX Y(ldy, lot)
// </srcblock> 
//
// <example>
// Example 1:  Initialize the TABLE array in preparation for doing an FFT
// of size 128.  Only the isign, n, and table arguments are used in this
// case.  You can use dummy arguments or zeros for the other arguments in
// the subroutine call.
//
// <srcblock> 
//       REAL TABLE(30 + 256)
//       CALL CCFFTM(0, 128, 0, 0., DUMMY, 1, DUMMY, 1, TABLE, DUMMY, 0)
// </srcblock> 
//
// Example 2:  X and Y are complex arrays of dimension (0:128) by (0:55).
// The first 128 elements of each column contain data.  For performance
// reasons, the extra element forces the leading dimension to be an odd
// number.  Take the FFT of the first 50 columns of X and store the
// results in the first 50 columns of Y.  Before taking the FFT,
// initialize the TABLE array, as in example 1.
//
// <srcblock> 
//       COMPLEX X(0:128, 0:55)
//       COMPLEX Y(0:128, 0:55)
//       REAL TABLE(30 + 256)
//       REAL WORK(256)
//       ...
//       CALL CCFFTM(0, 128, 50, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
//       CALL CCFFTM(1, 128, 50, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
// </srcblock> 
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/128 is used.  Assume that
// the TABLE array is already initialized.
//
// <srcblock> 
//       CALL CCFFTM(-1, 128, 50, 1./128., Y, 129, X, 129, TABLE,WORK,0)
// </srcblock> 
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  The
// subroutine calls are not changed.
//
// <srcblock> 
//       COMPLEX X(129, 55)
//       COMPLEX Y(129, 55)
//       ...
//       CALL CCFFTM(0, 128, 50, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
//       CALL CCFFTM(1, 128, 50, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
// </srcblock> 
//
// Example 5:  Perform the same computation as in example 4, but put the
// output back in array X to save storage space.  Assume that the TABLE
// array is already initialized.
//
// <srcblock> 
//       COMPLEX X(129, 55)
//       ...
//       CALL CCFFTM(1, 128, 50, 1.0, X, 129, X, 129, TABLE, WORK, 0)
// </srcblock> 
//
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n</b>
// <dd>    Integer.  
//         Size of each transform (the number of elements in each
//         column of the input and output matrix to be transformed).
//         Performance depends on the value of n, as explained above.  
//         n >= 0; if n = 0, the routine returns.
// <dt><b>lot</b>
// <dd>    Integer.
//         The number of transforms to be computed (lot size).  This is
//         the number of elements in each row of the input and output
//         matrix.  lot >= 0.  If lot = 0, the routine returns.
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>ccfftm</src>: real. 
//         <src>zzfftm</src>: double precision.
//         Each element of the output array is multiplied by scale
//         factor after taking the Fourier transform, as defined
//         previously.
// <dt><b>x</b>
// <dd>    Array of dimension (0:ldx-1, 0:n2-1).
//         <src>ccfftm</src>: real array.
//         <src>zzfftm</src>: double precision array.
//         Input array of values to be transformed.
// <dt><b>ldx</b>
// <dd>    The number of rows in the x array, as it was declared in the
//         calling program (the leading dimension of X).  ldx >= MAX(n, 1).
// <dt><b>ldy</b>
// <dd>    Integer.
//         The number of rows in the y array, as it was declared in the
//         calling program (the leading dimension of y).  ldy >= MAX(n,
//         1).
// <dt><b>isys</b>
// <dd>    Integer array of dimension (0:isys(0)).
//         The first element of the array specifies how many more
//         elements are in the array.  Use isys to specify certain
//         processor-specific parameters or options.
//
//         If isys(0) = 0, the default values of such parameters are
//         used.  In this case, you can specify the argument value as
//         the scalar integer constant 0.
//
//         If isys(0) > 0, isys(0) gives the upper bound of the isys
//         array.  Therefore, if il = isys(0), user-specified
//         parameters are expected in isys(1) through isys(il).
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Array of dimension (0:ldy-1, 0:lot-1).
//         <src>ccfftm</src>: complex array.
//         <src>zzfftm</src>: double complex array.
//         Output array of transformed values.  Each column of the
//         output array, y, is the FFT of the corresponding column of
//         the input array, x, computed according to the preceding
//         formula.
//
//         The output array may be the same as the input array. In that
//         case, the transform is done in place.  The input array is
//         overwritten with the transformed values.  In this case, it
//         is necessary that ldx = ldy.
// <dt><b>table</b>
// <dd>    Real array; dimension (30 + 2n).
//         Table of factors and trigonometric functions.
//
//         If isign = 0, the routine initializes table (table is output
//         only).
//
//         If isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0 (table is
//         input only).
// <dt><b>work</b>
// <dd>    Real array; dimension 2n.
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different from that
//         of the input and output arrays.
// </dl>
// <group>
static void ccfftm(Int isign, Int n, Int lot, Float scale, Complex*
		   x, Int ldx, Complex* y, Int ldy, Float* table,
		   Float* work, Int isys); 
static void zzfftm(Int isign, Int n, Int lot, Double scale, DComplex*
		   x, Int ldx, DComplex* y, Int ldy, Double* table,
		   Double* work, Int isys);
// </group>

// <src>scfftm/dzfftm</src> computes the FFT of each column of the real matrix
// X, and it stores the results in the corresponding column of the complex
// matrix Y.  <src>csfftm/zdfftm</src> computes the corresponding inverse
// transforms.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  First, the function of <src>scfftm</src> is
// described.  Suppose that the arrays are dimensioned as follows:
//
// <srcblock>
//      REAL    X(0:ldx-1, 0:lot-1)
//      COMPLEX Y(0:ldy-1, 0:lot-1)
//
// where ldx >= n, ldy >= n/2 + 1.
// </srcblock>
//
// Then column L of the output array is the FFT of column L of the input
// array, using the following formula for the FFT:
//
// <srcblock>
//                    n-1
// Y(k, L) = scale *  Sum  [ X(j, L)*w**(isign*j*k) ]
//                    j=0
//
// for k = 0, ..., n/2
//     L = 0, ..., lot-1 where:
//     w = exp(2*pi*i/n),
//     i = + sqrt(-1)
//     pi = 3.14159...,
//     isign = +1 or -1,
//     lot = the number of columns to transform
// </srcblock>
//
// Different authors use different conventions for which transform
// (isign = +1 or isign = -1) is used in the real-to-complex case, and
// what the scale factor should be.  Some adopt the convention that isign
// = 1 for the real-to-complex transform, and isign = -1 for the
// complex-to-real inverse.  Others use the opposite convention.  You can
// make these routines compute any of the various possible definitions,
// however, by choosing the appropriate values for isign and scale.
//
// The relevant fact from FFT theory is this:  If you use <src>scfftm</src> to
// take the real-to-complex FFT, using any particular values of isign and
// scale, the mathematical inverse function is computed by using 
// <src>csfftm</src> with -isign and 1/ (n*scale).  In particular, if you call
// <src>scfftm</src> with isign = +1 and scale = 1.0, you can use
// <src>csfftm</src> to compute the inverse complex-to-real FFT by using isign
// = -1 and scale = 1.0/n.
//
// <h3>Real-to-complex FFTs</h3>
// Notice in the preceding formula that there are n real input values and
// (n/2) + 1 complex output values for each column.  This property is
// characteristic of real-to-complex FFTs.
//
// The mathematical definition of the Fourier transform takes a sequence
// of n complex values and transforms it to another sequence of n complex
// values.  A complex-to-complex FFT routine, such as <src>ccfftm</src>, will
// take n complex input values and produce n complex output values.  In fact,
// one easy way to compute a real-to-complex FFT is to store the input
// data x in a complex array, then call routine <src>ccfftm</src> to compute
// the FFT.  You get the same answer when using the <src>scfftm</src> routine.
//
// A separate real-to-complex FFT routine is more efficient than the
// equivalent complex-to-complex routine.  Because the input data is
// real, you can make use of this fact to save almost half of the
// computational work.  According to the theory of Fourier transforms,
// for real input data, you have to compute only the first n/2 + 1
// complex output values in each column, because the second half of the
// FFT values in each column can be computed from the first half of the
// values by the simple formula:
//
// <srcblock>
//      Y    = conjgY          for n/2 <= k <= n-1
//       k,L          n-k, L
//
// where the notation conjg(z) represents the complex conjugate of z.
// </srcblock>
//
// In fact, in many applications, the second half of the complex output
// data is never explicitly computed or stored.  Likewise, you must
// supply only the first half of the complex data in each column has to
// be supplied for the complex-to-real FFT.
//
// Another implication of FFT theory is that for real input data, the
// first output value in each column, Y(0, L), will always be a real
// number; therefore, the imaginary part will always be 0.  If n is an
// even number, Y(n/2, L) will also be real and have 0 imaginary parts.
//
// <h3>Complex-to-real FFTs</h3>
// Consider the complex-to-real case.  The effect of the computation is
// given by the preceding formula, but with X complex and Y real.
//
// In general, the FFT transforms a complex sequence into a complex
// sequence; however, in a certain application you may know the output
// sequence is real, perhaps because the complex input sequence was the
// transform of a real sequence.  In this case, you can save about half
// of the computational work.
//
// According to the theory of Fourier transforms, for the output
// sequence, Y, to be a real sequence, the following identity on the
// input sequence, X, must be true:
//
// <srcblock>
//      X    = conjgX         for n/2 <= k <= n-1
//       k,L          n-k,L
// And, in fact, the following input values
//
//      X    for k > n/2
//       k,L
// do not have to be supplied, because they can be inferred from the
// first half of the input.
// </srcblock>
//
// Thus, in the complex-to-real routine, CSFFTM, the arrays can be
// dimensioned as follows:
//
// <srcblock>
//      COMPLEX X(0:ldx-1, 0:lot-1)
//      REAL    Y(0:ldy-1, 0:lot-1)
//
// where ldx >= n/2 + 1, ldy >= n.
// </srcblock>
//
// In each column, there are (n/2) + 1 complex input values and n real
// output values.  Even though only (n/2) + 1 input values are supplied,
// the size of the transform is still n in this case, because implicitly
// the FFT formula for a sequence of length n is used.
//
// Another implication of the theory is that X(0, L) must be a real
// number (that is, must have zero imaginary part).  If n is an even
// number, X(n/2, L) must also be real.  Routine CSFFTM assumes that each
// of these values is real; if a nonzero imaginary part is given, it is
// ignored.
//
// <h3>Table Initialization</h3>
// The table array contains the trigonometric tables used in calculation
// of the FFT.  You must initialize this table by calling the routine
// with isign = 0 prior to doing the transforms.  table does not have to
// be reinitialized if the value of the problem size, n, does not change.
//
// <h3>Dimensions</h3>
// In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared (for SCFFTM):
//
// <srcblock>
//      REAL    X(0:ldx-1, 0:lot-1)
//      COMPLEX Y(0:ldy-1, 0:lot-1)
// </srcblock>
//
// No change is made in the calling sequence, however, if you prefer to
// use the more customary Fortran style with subscripts starting at 1.
// The same values of ldx and ldy would be passed to the subroutine even
// if the input and output arrays were dimensioned as follows:
//
// <srcblock>
//      REAL    X(ldx, lot)
//      COMPLEX Y(ldy, lot)
// </srcblock>

// </example>
// Example 1:  Initialize the complex array TABLE in preparation for
// doing an FFT of size 128.  In this case only the isign, n, and table
// arguments are used; you may use dummy arguments or zeros for the other
// arguments in the subroutine call.
//
// <srcblock>
//       REAL TABLE(15 + 128)
//       CALL SCFFTM(0, 128, 1, 0.0, DUMMY, 1, DUMMY, 1,
//      &  TABLE, DUMMY, 0)
// </srcblock>
//
// Example 2:  X is a real array of dimension (0:128, 0:55), and Y is a
// complex array of dimension (0:64, 0:55).  The first 128 elements in
// each column of X contain data; the extra element forces an odd leading
// dimension.  Take the FFT of the first 50 columns of X and store the
// results in the first 50 columns of Y.  Before taking the FFT,
// initialize the TABLE array, as in example 1.
//
// <srcblock>
//       REAL    X(0:128, 0:55)
//       COMPLEX Y(0:64,  0:55)
//       REAL    TABLE(15 + 128)
//       REAL    WORK((128)
//       ...
//       CALL SCFFTM(0, 128, 50, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
//       CALL SCFFTM(1, 128, 50, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
// </srcblock>
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/128 is used.  Assume that
// the TABLE array is initialized already.
//
// <srcblock>
//       CALL CSFFTM(-1, 128, 50, 1.0/128.0, Y, 65, X, 129,
//      &  TABLE, WORK, 0)
// </srcblock>
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  No change is
// made in the subroutine calls.
//
// <srcblock>
//       REAL    X(129, 56)
//       COMPLEX Y(65, 56)
//       ...
//       CALL SCFFTM(0, 128, 50, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
//       CALL SCFFTM(1, 128, 50, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
// </srcblock>
//
// Example 5:  Perform the same computation as in example 4, but
// equivalence the input and output arrays to save storage space.  In
// this case, a row must be added to X, because it is equivalenced to a
// complex array.  The leading dimension of X is two times an odd number;
// therefore, memory bank conflicts are minimal.  Assume that TABLE is
// initialized already.
//
// <srcblock>
//       REAL    X(130, 56)
//       COMPLEX Y(65, 56)
//       EQUIVALENCE ( X(1, 1), Y(1, 1) )
//       ...
//       CALL SCFFTM(1, 128, 50, 1.0, X, 130, Y, 65, TABLE, WORK, 0)
// </srcblock>
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n</b>
// <dd>    Integer.
//         Size of the transforms (the number of elements in each
//         column of the input and output matrix to be transformed).
//         If n is not positive, <src>scfftm</src> or <src>csfftm</src> returns
//         without computing a transforms.
// <dt><b>lot</b>
// <dd>    Integer.
//         The number of transforms to be computed (or "lot size").
//         This is the number of elements in each row of the input and
//         output matrix.  If lot is not positive, <src>csfftm</src> or 
//         <src>scfftm</src> returns without computing a transforms.
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>scfftm</src>: real. 
//         <src>dzfftm</src>: double precision.
//         <src>csfftm</src>: real.
//         <src>zdfftm</src>: double precision.
//         Each element of the output array is multiplied by scale
//         after taking the transform, as defined in the preceding
//         formula.
// <dt><b>x</b>
// <dd>    Input array of values to be transformed. Dimension (0:ldx-1,
//         0:lot-1).
//         <src>scfftm</src>: real array.
//         <src>dzfftm</src>: double precision array.
//         <src>csfftm</src>: complex array.
//         <src>zdfftm</src>: double complex array.
// <dt><b>ldx</b>
// <dd>    Integer.
//         The number of rows in the x array, as it was declared in the
//         calling program.  That is, the leading dimension of x.
//         <src>scfftm, dzfftm</src>:  ldx >= MAX(n, 1).
//         <src>csfftm, zdfftm</src>:  ldx >= MAX(n/2 + 1, 1).
// <dt><b>ldy</b>
// <dd>    Integer.
//         The number of rows in the y array, as it was declared in the
//         calling program (the leading dimension of y).
//         <src>scfftm, dzfftm</src>:  ldy >= MAX(n/2 + 1, 1).
//         <src>csfftm, zdfftm</src>:  ldy >= MAX(n, 1).
// <dt><b>isys</b>
// <dd>    Integer array of dimension (0:isys(0)).
//         The first element of the array specifies how many more
//         elements are in the array.  Use isys to specify certain
//         processor-specific parameters or options.
//
//         If isys(0) = 0, the default values of such parameters are
//         used.  In this case, you can specify the argument value as
//         the scalar integer constant 0.
//
//         If isys(0) > 0, isys(0) gives the upper bound of the isys
//         array.  Therefore, if il = isys(0), user-specified
//         parameters are expected in isys(1) through isys(il).
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Output array of transformed values.  Dimension (0:ldy-1,
//         0:lot-1).
//         <src>scfftm</src>: complex array.
//         <src>dzfftm</src>: double complex array.
//         <src>csfftm</src>: real array.
//         <src>zdfftm</src>: double precision array.
//
//         Each column of the output array, y, is the FFT of the
//         corresponding column of the input array, x, computed
//         according to the preceding formula.  The output array may be
//         equivalenced to the input array. In that case, the transform
//         is done in place and the input array is overwritten with the
//         transformed values.  In this case, the following conditions
//         on the leading dimensions must hold:
//
//         <src>scfftm, dzfftm</src>:  ldx = 2ldy.
//         <src>csfftm, zdfftm</src>:  ldy = 2ldx.
// <dt><b>table</b>
// <dd>    Real array; dimension (15 + n).
//         Table of factors and trigonometric functions.
//         This array must be initialized by a call to <src>scfftm</src> (or
//         <src>csfftm</src>) with isign = 0.
//
//         If isign = 0, table is initialized to contain trigonometric
//         tables needed to compute an FFT of length n.
// <dt><b>work</b>
// <dd>    Real array; dimension n.
//         Work array used for intermediate calculations.  Its address
//         space must be different from that of the input and output
//         arrays.
// </dl>
// <group>
static void scfftm(Int isign, Int n, Int lot, Float scale, Float*
		   x, Int ldx, Complex* y, Int ldy, Float* table,
		   Float* work, Int isys); 
static void dzfftm(Int isign, Int n, Int lot, Double scale, Double*
		   x, Int ldx, DComplex* y, Int ldy, Double* table,
		   Double* work, Int isys); 
static void csfftm(Int isign, Int n, Int lot, Float scale, Complex*
		   x, Int ldx, Float* y, Int ldy, Float* table,
		   Float* work, Int isys); 
static void zdfftm(Int isign, Int n, Int lot, Double scale, DComplex*
		   x, Int ldx, Double* y, Int ldy, Double* table,
		   Double* work, Int isys);
// </group>

// These routines compute the two-dimensional complex Fast Fourier
// Transform (FFT) of the complex matrix x, and store the results in the
// complex matrix y.  <src>ccfft2d</src> does the complex-to-complex
// transform and <src>zzfft</src> does the same for double
// precision arrays.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  Suppose that the arrays are
// dimensioned as follows:
//
// <srcblock> 
//      COMPLEX X(0:n1-1, 0:n2-1)
//      COMPLEX Y(0:n1-1, 0:n2-1)
// </srcblock> 
//
// These routines compute the formula:
//
// <srcblock> 
//                        n2-1  n1-1
//    Y(k1, k2) = scale * Sum   Sum [ X(j1, j2)*w1**(j1*k1)*w2**(j2*k2) ]
//                        j2=0  j1=0
//
//    for k1 = 0, ..., n1-1
//        k2 = 0, ..., n2-1
//
//    where:
//        w1 = exp(isign*2*pi*i/n1)
//        w2 = exp(isign*2*pi*i/n2)
//        i = + sqrt(-1)
//        pi = 3.14159...,
//        isign = +1 or -1
// </srcblock> 
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make this routine compute any of the various possible definitions,
// however, by choosing the appropriate values for isign and scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and
// 1/(n1*n2*scale).  In particular, if you use isign = +1 and scale = 1.0
// for the forward FFT, you can compute the inverse FFT by using isign =
// -1 and scale = 1.0/(n1*n2).
//
// <h3>Algorithm</h3>
// These routines use a routine very much like <src>ccfftm/zzfftm</src> to do
// multiple FFTs first on all columns in an input matrix and then on all
// of the rows.
//
// <h3>Initialization</h3>
// The table array stores factors of n1 and n2 and also trigonometric
// tables that are used in calculation of the FFT.  This table must be
// initialized by calling the routine with isign = 0.  If the values of
// the problem sizes, n1 and n2, do not change, the table does not have
// to be reinitialized.
//
// <h3>Dimensions</h3>
// In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared as follows:
//
// <srcblock> 
//      COMPLEX X(0:ldx-1, 0:n2-1)
//      COMPLEX Y(0:ldy-1, 0:n2-1)
// </srcblock> 
//
// However, the calling sequence does not change if you prefer to use the
// more customary Fortran style with subscripts starting at 1.  The same
// values of ldx and ldy would be passed to the subroutine even if the
// input and output arrays were dimensioned as follows:
//
// <srcblock> 
//      COMPLEX X(ldx, n2)
//      COMPLEX Y(ldy, n2)
// </srcblock> 
//
// <example>
// All examples here are for Origin series only.
//
// Example 1:  Initialize the TABLE array in preparation for doing a
// two-dimensional FFT of size 128 by 256.  In this case only the isign,
// n1, n2, and table arguments are used; you can use dummy arguments or
// zeros for other arguments.
//
// <srcblock> 
//        REAL TABLE ((30 + 256) + (30 + 512))
//        CALL CCFFT2D (0, 128, 256, 0.0, DUMMY, 1, DUMMY, 1,
//       &  TABLE, DUMMY, 0)
// </srcblock> 
//
// Example 2:  X and Y are complex arrays of dimension (0:128, 0:255).
// The first 128 elements of each column contain data.  For performance
// reasons, the extra element forces the leading dimension to be an odd
// number.  Take the two-dimensional FFT of X and store it in Y.
// Initialize the TABLE array, as in example 1.
//
// <srcblock> 
//       COMPLEX X(0:128, 0:255)
//       COMPLEX Y(0:128, 0:255)
//       REAL     TABLE((30 + 256) + (30 + 512))
//       REAL     WORK(2*128*256)
//       ...
//       CALL CCFFT2D(0, 128, 256, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
//       CALL CCFFT2D(1, 128, 256, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
// </srcblock> 
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/(128*256) is used.  Assume
// that the TABLE array is already initialized.
//
// <srcblock> 
//       CALL CCFFT2D(-1, 128, 256, 1.0/(128.0*256.0), Y, 129,
//      &  X, 129, TABLE, WORK, 0)
// </srcblock> 
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  The
// subroutine calls are not changed.
//
// <srcblock> 
//       COMPLEX X(129, 256)
//       COMPLEX Y(129, 256)
//       ...
//       CALL CCFFT2D(0, 128, 256, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
//       CALL CCFFT2D(1, 128, 256, 1.0, X, 129, Y, 129, TABLE, WORK, 0)
// </srcblock> 
//
// Example 5:  Perform the same computation as in example 4, but put the
// output back in array X to save storage space.  Assume that the TABLE
// array is already initialized.
//
// <srcblock> 
//       COMPLEX X(129, 256)
//       ...
//       CALL CCFFT2D(1, 128, 256, 1.0, X, 129, X, 129, TABLE, WORK, 0)
// </srcblock> 
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse transform as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n1, n2, table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n1</b>
// <dd>    Integer.
//         Transform size in the first dimension.  If n1 is not
//         positive, the routine returns without performing a
//         transform.
// <dt><b>n2</b>
// <dd>    Integer.
//         Transform size in the second dimension.  If n2 is not
//         positive, the routine returns without performing a
//         transform.
// <dt><b>scale</b>
// <dd>    Scale factor.
//         ccfft2d: real.
//         zzfft2d: double precision.
//         Each element of the output array is multiplied by scale
//         factor after taking the Fourier transform, as defined
//         previously.
// <dt><b>x</b>
// <dd>    Array of dimension (0:ldx-1, 0:n2-1).
//         ccfft2d: complex array.
//         zzfft2d: double complex array.
//         Input array of values to be transformed.
// <dt><b>ldx</b>
// <dd>    Integer.
//         The number of rows in the x array, as it was declared in the
//         calling program (the leading dimension of x).  ldx >=
//         MAX(n1, 1).
// <dt><b>ldy</b>
// <dd>    Integer.
//
//         The number of rows in the y array, as it was declared in the
//         calling program (the leading dimension of y).  ldy >=
//         MAX(n1, 1).
// <dt><b>isys</b>
// <dd>    Algorithm used; value dependent on hardware system.  Currently, no
//         special options are supported; therefore, you must always specify
//         an isys argument as constant 0.  
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Array of dimension (0:ldy-1, 0:n2-1).
//         ccfft2d: complex array.
//         zzfft2d: double complex array.
//         Output array of transformed values.  The output array may be
//         the same as the input array, in which case, the transform is
//         done in place (the input array is overwritten with the
//         transformed values).  In this case, it is necessary that
//         ldx = ldy.
// <dt><b>table</b>
// <dd>    Real array; dimension (30+ 2 * n1) + (30 + 2 * n2).
//
//         Table of factors and trigonometric functions.
//
//         If isign = 0, the routine initializes table (table is output
//         only).
//
//         If isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0 (table is
//         input only).
// <dt><b>work</b>
// <dd>    Real array; dimension 2 * (n1*n2).
//
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different from that
//         of the input and output arrays.
// </dl>
// <group>
static void ccfft2d(Int isign, Int n1, Int n2, Float scale, Complex*
		    x, Int ldx, Complex* y, Int ldy, Float* table,
		    Float* work, Int isys); 
static void zzfft2d(Int isign, Int n1, Int n2, Double scale, DComplex*
		    x, Int ldx, DComplex* y, Int ldy, Double* table,
		    Double* work, Int isys);
// </group>
 
// <src>scfft2d/dzfft2d</src> computes the two-dimensional Fast Fourier
// Transform (FFT) of the real matrix x, and it stores the results in the
// complex matrix y.  <src>csfft2d/zdfft2d</src> computes the corresponding
// inverse transform.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  First the function of <src>scfft2d</src> is
// described.  Suppose the arrays are dimensioned as follows:
//
// <srcblock>
//      REAL    X(0:ldx-1, 0:n2-1)
//      COMPLEX Y(0:ldy-1, 0:n2-1)
//
// where ldx >= n1 ldy >= (n1/2) + 1.
// </srcblock>
//
// <src>scfft2d</src> computes the formula:
//
// <srcblock>
//                         n2-1 n1-1
//     Y(k1, k2) = scale * Sum  Sum [ X(j1, j2)*w1**(j1*k1)*w2**(j2*k2) ]
//                         j2=0 j1=0
//
//     for k1 = 0, ..., n1/2 + 1
//         k2 = 0, ..., n2-1
//
//     where:
//         w1 = exp(isign*2*pi*i/n1)
//         w2 = exp(isign*2*pi*i/n2)
//         i  = + sqrt(-1)
//         pi = 3.14159...,
//         isign = +1 or -1
// </srcblock>
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make these routines compute any of the various possible
// definitions, however, by choosing the appropriate values for isign and
// scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and 1/(n1 * n2 *
// scale).  In particular, if you use isign = +1 and scale = 1.0 for the
// forward FFT, you can compute the inverse FFT by using isign = -1 and
// scale = 1.0/(n1 . n2).
//
// <src>scfft2d</src> is very similar in function to <src>ccfft2d</src>, but
// it takes the real-to-complex transform in the first dimension, followed by
// the complex-to-complex transform in the second dimension.
//
// <src>csfft2d</src> does the reverse.  It takes the complex-to-complex FFT
// in the second dimension, followed by the complex-to-real FFT in the first
// dimension.
//
// See the <src>scfft</src> man page for more information about real-to-complex
// and complex-to-real FFTs.  The two-dimensional analog of the conjugate
// formula is as follows:
//
// <srcblock>
//      Y       = conjg Y
//       k , k            n1 - k , n2 - k
//        1   2                 1        2
//
//      for n1/2 <  k  <= n1 - 1
//                   1
//
//         0 <= k  <= n2 - 1
//               2
// where the notation conjg(z) represents the complex conjugate of z.
// </srcblock>
//
// Thus, you have to compute only (slightly more than) half of the output
// values, namely:
//
// <srcblock>
//      Y       for 0 <= k  <= n1/2    0 <= k  <= n2 - 1
//       k , k            1                  2
//        1   2
// </srcblock>
//
// <h3>Algorithm</h3>
// <src>scfft2d</src> uses a routine similar to <src>scfftm</src> to do a
// real-to-complex FFT on the columns, then uses a routine similar to
// <src>ccfftm</src> to do a complex-to-complex FFT on the rows.
//
// <src>csfft2d</src> uses a routine similar to <src>ccfftm</src> to do a
// complex-to-complex FFT on the rows, then uses a routine similar to
// <src>csfftm</src> to do a complex-to-real FFT on the columns.
//
// <h3>Table Initialization</h3>
// The table array stores factors of n1 and n2, and trigonometric tables
// that are used in calculation of the FFT.  table must be initialized by
// calling the routine with isign = 0.  table does not have to be
// reinitialized if the values of the problem sizes, n1 and n2, do not
// change.
//
// <h3>Dimensions</h3>
// In the preceding description, it is assumed that array subscripts were
// zero-based, as is customary in FFT applications.  Thus, the input and
// output arrays are declared:
//
// <srcblock>
//      REAL    X(0:ldx-1, 0:n2-1)
//      COMPLEX Y(0:ldy-1, 0:n2-1)
// </srcblock>
//
// No change is made in the calling sequence, however, if you prefer to
// use the more customary Fortran style with subscripts starting at 1.
// The same values of ldx and ldy would be passed to the subroutine even
// if the input and output arrays were dimensioned as follows:
//
// <srcblock>
//      REAL    X(ldx, n2)
//      COMPLEX Y(ldy, n2)
// </srcblock>
//
// <example>
// The following examples are for Origin series only.
//
// Example 1:  Initialize the TABLE array in preparation for doing a
// two-dimensional FFT of size 128 by 256.  In this case, only the isign,
// n1, n2, and table arguments are used; you can use dummy arguments or
// zeros for other arguments.
//
// <srcblock>
//       REAL TABLE ((15 + 128) + 2(15 + 256))
//       CALL SCFFT2D (0, 128, 256, 0.0, DUMMY, 1, DUMMY, 1,
//      &  TABLE, DUMMY, 0)
// </srcblock>
//
// Example 2:  X is a real array of size (0:128, 0: 255), and Y is a
// complex array of dimension (0:64, 0:255).  The first 128 elements of
// each column of X contain data; for performance reasons, the extra
// element forces the leading dimension to be an odd number.  Take the
// two-dimensional FFT of X and store it in Y.  Initialize the TABLE
// array, as in example 1.
//
// <srcblock>
//       REAL    X(0:128, 0:255)
//       COMPLEX Y(0:64, 0:255)
//       REAL    TABLE ((15 + 128) + 2(15 + 256))
//       REAL    WORK(128*256)
//       ...
//       CALL SCFFT2D(0, 128, 256, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
//       CALL SCFFT2D(1, 128, 256, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
// </srcblock>
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/(128*256) is used.  Assume
// that the TABLE array is initialized already.
//
// <srcblock>
//       CALL CSFFT2D(-1, 128, 256, 1.0/(128.0*256.0), Y, 65,
//      &  X, 130, TABLE, WORK, 0)
// </srcblock>
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  No change is
// needed in the subroutine calls.
//
// <srcblock>
//       REAL    X(129, 256)
//       COMPLEX Y(65, 256)
//       ...
//       CALL SCFFT2D(0, 128, 256, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
//       CALL SCFFT2D(1, 128, 256, 1.0, X, 129, Y, 65, TABLE, WORK, 0)
// </srcblock>
//
// Example 5:  Perform the same computation as in example 4, but
// equivalence the input and output arrays to save storage space.  In
// this case, a row must be added to X, because it is equivalenced to a
// complex array.  Assume that TABLE is already initialized.
//
// <srcblock>
//       REAL    X(130, 256)
//       COMPLEX Y(65, 256)
//       EQUIVALENCE ( X(1, 1), Y(1, 1) )
//       ...
//       CALL SCFFT2D(1, 128, 256, 1.0, X, 130, Y, 65, TABLE, WORK, 0)
// </srcblock>
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
// <dt><b>n1</b>
// <dd>    Integer.
//         Transform size in the first dimension.  If n1 is not
//         positive, <src>scfft2d</src> returns without calculating a
//         transform.
// <dt><b>n2</b>
// <dd>    Integer.
//         Transform size in the second dimension.  If n2 is not
//         positive, <src>scfft2d</src> returns without calculating a
//         transform.
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>scfft2d</src>: real. 
//         <src>dzfft2d</src>: double precision.
//         <src>csfft2d</src>: real.
//         <src>zdfft2d</src>: double precision.
//         Each element of the output array is multiplied by scale
//         factor after taking the Fourier transform, as defined
//         previously.
// <dt><b>x</b>
// <dd>    Array of dimension (0:ldx-1, 0:n2-1).
//         <src>scfft2d</src>: real array.
//         <src>dzfft2d</src>: double precision array.
//         <src>csfft2d</src>: complex array.
//         <src>zdfft2d</src>: double complex array.
//
//         Array of values to be transformed.
// <dt><b>ldx</b>
// <dd>    Integer.
//         The number of rows in the x array, as it was declared in the
//         calling program.  That is, the leading dimension of x.
//         <src>scfft2d, dzfft2d</src>:  ldx >= MAX(n1, 1).
//         <src>csfft2d, zdfft2d</src>:  ldx >= MAX(n1/2 + 1, 1).
// <dt><b>ldy</b>
// <dd>    Integer.
//
//         The number of rows in the y array, as it was declared in the
//         calling program (the leading dimension of y).
//
//         <src>scfft2d, dzfft2d</src>:  ldy >= MAX(n1/2 + 1, 1).
//         <src>csfft2d, zdfft2d</src>:  ldy >= MAX(n1 + 2, 1).
//
//         In the complex-to-real routine, two extra elements are in
//         the first dimension (ldy >= n1 + 2, rather than just ldy >=
//         n1).  These elements are needed for intermediate storage
//         during the computation.  On exit, their value is undefined.
// <dt><b>isys</b>
// <dd>    Integer array of dimension (0:isys(0)).
//         The first element of the array specifies how many more
//         elements are in the array.  Use isys to specify certain
//         processor-specific parameters or options.
//
//         If isys(0) = 0, the default values of such parameters are
//         used.  In this case, you can specify the argument value as
//         the scalar integer constant 0.
//
//         If isys(0) > 0, isys(0) gives the upper bound of the isys
//         array.  Therefore, if il = isys(0), user-specified
//         parameters are expected in isys(1) through isys(il).
// <dt><b>isys</b>
// <dd>    Algorithm used; value dependent on hardware system.  Currently, no
//         special options are supported; therefore, you must always specify
//         an isys argument as constant 0.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    <src>scfft2d</src>: complex array.
//         <src>dzfft2d</src>: double complex array.
//         <src>csfft2d</src>: real array.
//         <src>zdfft2d</src>: double precision array.
//
//         Output array of transformed values.  The output array can be
//         the same as the input array, in which case, the transform is
//         done in place and the input array is overwritten with the
//         transformed values.  In this case, it is necessary that the
//         following equalities hold:
//
//         <src>scfft2d, dzfft2d</src>:  ldx = 2 * ldy.
//         <src>csfft2d, zdfft2d</src>:  ldy = 2 * ldx.
// <dt><b>table</b>
// <dd>    Real array; dimension (15 + n1) + 2(15 + n2).
//
//         Table of factors and trigonometric functions.
//
//         If isign = 0, the routine initializes table (table is output
//         only).
//
//         If isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0 (table is
//         input only).
// <dt><b>work</b>
// <dd>    Real array; dimension (n1 * n2).
//
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different from that
//         of the input and output arrays.
// </dl>
// <group>
static void scfft2d(Int isign, Int n1, Int n2, Float scale, Float*
		    x, Int ldx, Complex* y, Int ldy, Float* table,
		    Float* work, Int isys); 
static void dzfft2d(Int isign, Int n1, Int n2, Double scale, Double*
		    x, Int ldx, DComplex* y, Int ldy, Double* table,
		    Double* work, Int isys); 
static void csfft2d(Int isign, Int n1, Int n2, Float scale, Complex*
		    x, Int ldx, Float* y, Int ldy, Float* table,
		    Float* work, Int isys); 
static void zdfft2d(Int isign, Int n1, Int n2, Double scale, DComplex*
		    x, Int ldx, Double* y, Int ldy, Double* table,
		    Double* work, Int isys); 
// </group>

// These routines compute the three-dimensional complex FFT of the
// complex matrix X, and store the results in the complex matrix Y.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  So suppose the arrays are dimensioned
// as follows:
//
// <srcblock>
//      COMPLEX X(0:n1-1, 0:n2-1, 0:n3-1)
//      COMPLEX Y(0:n1-1, 0:n2-1, 0:n3-1)
// </srcblock>
//
// These routines compute the formula:
//
// <srcblock>
//    Y(k1,k2,k3) =
//        n1-1 n2-1 n3-1
//    scale * Sum  Sum  Sum [X(j1,j2,j3)*w1**(j1*k1)*w2**(j2*k2)*w3**(j3*k3)]
//        j1=0 j2=0 j3=0
//
//    for k1 = 0, ..., n1 - 1,
//    k2 = 0, ..., n2 - 1,
//    k3 = 0, ..., n3 - 1,
//
//    where:
//    w1 = exp(isign*2*pi*i/n1),
//    w2 = exp(isign*2*pi*i/n2),
//    w3 = exp(isign*2*pi*i/n3),
//    i  = + sqrt(-1)
//    pi = 3.14159...
//    isign = +1 or -1
// </srcblock>
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make this routine compute any of the various possible definitions,
// however, by choosing the appropriate values for isign and scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and 1/(n1 * n2 * n3
// * scale).  In particular, if you use isign = +1 and scale = 1.0 for
// the forward FFT, you can compute the inverse FFT by using isign = -1
// and scale = 1/(n1 . n2 . n3).
//
// <example>
// The following examples are for Origin series only.
//
// Example 1:  Initialize the TABLE array in preparation for doing a
// three-dimensional FFT of size 128 by 128 by 128.  In this case, only
// the isign, n1, n2, n3, and table arguments are used; you can use dummy
// arguments or zeros for other arguments.
//
// <srcblock>
//       REAL TABLE ((30 + 256) + (30 + 256) + (30 + 256))
//       CALL CCFFT3D (0, 128, 128, 128, 0.0, DUMMY, 1, 1, DUMMY, 1, 1,
//      &  TABLE, DUMMY, 0)
// </srcblock>
//
// Example 2:  X and Y are complex arrays of dimension (0:128, 0:128,
// 0:128).  The first 128 elements of each dimension contain data; for
// performance reasons, the extra element forces the leading dimensions
// to be odd numbers.  Take the three-dimensional FFT of X and store it
// in Y.  Initialize the TABLE array, as in example 1.
//
// <srcblock>
//       COMPLEX X(0:128, 0:128, 0:128)
//       COMPLEX Y(0:128, 0:128, 0:128)
//       REAL TABLE ((30+256) + (30 + 256) + (30 + 256))
//       REAL     WORK 2(128*128*128)
//       ...
//       CALL CCFFT3D(0, 128, 128, 128, 1.0, DUMMY, 1, 1,
//     &    DUMMY, 1, 1, TABLE, WORK, 0)
//       CALL CCFFT3D(1, 128, 128, 128, 1.0, X, 129, 129,
//     &    Y, 129, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1.0/(128.0**3) is used.
// Assume that the TABLE array is already initialized.
//
// <srcblock>
//       CALL CCFFT3D(-1, 128, 128, 128, 1.0/(128.0**3), Y, 129, 129,
//      &  X, 129, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  The
// subroutine calls do not change.
//
// <srcblock>
//       COMPLEX X(129, 129, 129)
//       COMPLEX Y(129, 129, 129)
//       ...
//       CALL CCFFT3D(0, 128, 128, 128, 1.0, DUMMY, 1, 1,
//      &    DUMMY, 1, 1, TABLE, WORK, 0)
//       CALL CCFFT3D(1, 128, 128, 128, 1.0, X, 129, 129,
//      &    Y, 129, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 5:  Perform the same computation as in example 4, but put the
// output back in the array X to save storage space.  Assume that the
// TABLE array is already initialized.
//
// <srcblock>
//       COMPLEX X(129, 129, 129)
//       ...
//       CALL CCFFT3D(1, 128, 128, 128, 1.0, X, 129, 129,
//      &    X, 129, 129, TABLE, WORK, 0)
// </srcblock>
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n1, n2, n3, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
//
// <dt><b>n1</b>
// <dd>    Integer.
//         Transform size in the first dimension.  If n1 is not
//         positive, the routine returns without computing a transform.
//
// <dt><b>n2</b>
// <dd>    Integer.
//         Transform size in the second dimension.  If n2 is not
//         positive, the routine returns without computing a transform.
//
// <dt><b>n3</b>
// <dd>    Integer.
//         Transform size in the third dimension.  If n3 is not
//         positive, the routine returns without computing a transform.
//
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>ccfft3d</src>: real.
//         <src>zzfft3d</src>: double precision.
//
//         Each element of the output array is multiplied by scale
//         after taking the Fourier transform, as defined previously.
//
// <dt><b>x</b>
// <dd>    Array of dimension (0:ldx-1, 0:ldx2-1, 0:n3-1).
//         <src>ccfft3d</src>: complex array.
//         <src>zzfft3d</src>: double complex array.
//
//         Input array of values to be transformed.
//
// <dt><b>ldx</b>
// <dd>    Integer.
//         The first dimension of x, as it was declared in the calling
//         program (the leading dimension of x).  ldx >= MAX(n1, 1).
//
// <dt><b>ldx2</b>
// <dd>    Integer.
//         The second dimension of x, as it was declared in the calling
//         program.  ldx2 >= MAX(n2, 1).
//
// <dt><b>ldy</b>
// <dd>    Integer.
//         The first dimension of y, as it was declared in the calling
//         program (the leading dimension of y).  ldy >= MAX(n1, 1).
//
// <dt><b>ldy2</b>
// <dd>    Integer.
//         The second dimension of y, as it was declared in the calling
//         program.  ldy2 >= MAX(n2, 1).
//
// <dt><b>isys</b>
// <dd>    Algorithm used; value dependent on hardware system.  Currently, no
//         special options are supported; therefore, you must always specify
//         an isys argument as constant 0.
//
//         isys = 0 or 1 depending on the amount of workspace the user
//         can provide to the routine.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Array of dimension (0:ldy-1, 0:ldy2-1, 0:n3-1).
//         <src>ccfft3d</src>: complex array.
//         <src>zzfft3d</src>: double complex array.
//
//         Output array of transformed values.  The output array may be
//         the same as the input array, in which case, the transform is
//         done in place; that is, the input array is overwritten with
//         the transformed values.  In this case, it is necessary that
//         ldx = ldy, and ldx2 = ldy2.
//
// <dt><b>table</b>
// <dd>    Real array; dimension 30 + 2 * n1) + (30 + 2 * n2) + (30 + 2 * n3).
//
//         Table of factors and trigonometric functions.  If isign = 0,
//         the routine initializes table (table is output only).  If
//         isign = +1 or -1, the values in table are assumed to be
//         initialized already by a prior call with isign = 0 (table is
//         input only).
//
// <dt><b>work</b>
// <dd>    Real array; dimension (n1 * n2 * n3).
//
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different from that
//         of the input and output arrays.
//
// </dl>
// <group>
static void ccfft3d(Int isign, Int n1, Int n2, Int n3, Float scale,
		    Complex* x, Int ldx, Int ldx2, Complex* y, Int ldy,
		    Int ldy2, Float* table, Float* work, Int isys);
static void zzfft3d(Int isign, Int n1, Int n2, Int n3, Double scale,
		    DComplex* x, Int ldx, Int ldx2, DComplex* y, Int
		    ldy, Int ldy2, Double* table, Double* work, Int
		    isys); 
// </group>

// These are C++ wrapper functions for the 3D real-to-complex and
// complex-to-real transform routines in the SGI/Cray Scientific Library
// (SCSL). The purpose of these definitions is to overload the functions so
// that C++ users can access the functions in SCSL with identical function
// names.
//
// <note role=warning> 
// Currently, the SCSL is available only on SGI machines.
// </note>
//
// <src>scfft3d/dzfft3d</src> computes the three-dimensional Fast Fourier
// Transform (FFT) of the real matrix X, and it stores the results in the
// complex matrix Y.  <src>csfft3d/zdfft3d</src> computes the corresponding
// inverse transform.
//
// In FFT applications, it is customary to use zero-based subscripts; the
// formulas are simpler that way.  First, the function of <src>SCFFT3D</src> is
// described.  Suppose the arrays are dimensioned as follows:
//
// <srcblock>
//      REAL    X(0:ldx-1, 0:ldx2-1, 0:n3-1)
//      COMPLEX Y(0:ldy-1, 0:ldy2-1, 0:n3-1)
// </srcblock>
//
// <src>scfft3d</src> computes the formula:
//
// <srcblock>
//    Y(k1,k2,k3) =
//        n1-1 n2-1 n3-1
//    scale * Sum  Sum  Sum [X(j1,j2,j3)*w1**(j1*k1)*w2**(j2*k2)*w3**(j3*k3)]
//        j1=0 j2=0 j3=0
//
//    for k1 = 0, ..., n1/2,
//        k2 = 0, ..., n2 - 1,
//        k3 = 0, ..., n3 - 1,
//
//    where:
//        w1 = exp(isign*2*pi*i/n1),
//        w2 = exp(isign*2*pi*i/n2),
//        w3 = exp(isign*2*pi*i/n3),
//        i = + sqrt(-1)
//        pi = 3.14159...
//        isign = +1 or -1
// </srcblock>
//
// Different authors use different conventions for which of the
// transforms, isign = +1 or isign = -1, is the forward or inverse
// transform, and what the scale factor should be in either case.  You
// can make these routines compute any of the various possible
// definitions, however, by choosing the appropriate values for isign and
// scale.
//
// The relevant fact from FFT theory is this:  If you take the FFT with
// any particular values of isign and scale, the mathematical inverse
// function is computed by taking the FFT with -isign and 1/(n1 * n2 * n3
// * scale).  In particular, if you use isign = +1 and scale = 1.0 for
// the forward FFT, you can compute the inverse FFT by isign = -1 and
//
// <srcblock>
//      scale = 1.0/(n1*n2*n3).
// </srcblock>
//
// <src>scfft3d</src> is very similar in function to <src>ccfft3d</src>, but
// it takes the real-to-complex transform in the first dimension, followed by
// the complex-to-complex transform in the second and third dimensions.
//
// <src>csfft3d</src> does the reverse.  It takes the complex-to-complex FFT
// in the third and second dimensions, followed by the complex-to-real FFT in
// the first dimension.
//
// See the <src>scfftm</src> man page for more information about
// real-to-complex and complex-to-real FFTs.  The three dimensional analog of
// the conjugate formula is as follows:
//
// <srcblock>
//      Y         = conjg Y
//       k ,k ,k            n1 - k , n2 - k , n3 - k
//        1  2  3                 1        2        3
//
//      for  n1/2 <  k  <= n1 - 1
//                    1
//
//           0 <= k  <= n2 - 1
//                 2
//
//           0 <= k  <= n3 - 1
//                 3
// where the notation conjg(z) represents the complex conjugate of z.
// </srcblock>
//
// Thus, you have to compute only (slightly more than) half out the
// output values, namely:
//
// <srcblock>
//      Y
//       k ,k ,k
//        1  2  3
//
//      for  0 <= k  <= n1/2
//                 1
//
//           0 <= k  <= n2 - 1
//                 2
//
//           0 <= k  <= n3 - 1
// </srcblock>
//
// <h3>Algorithm</h3>
// <src>scfft3d</src> uses a routine similar to <src>scfftm</src> to do
// multiple FFTs first on all columns of the input matrix, then uses a routine
// similar to <src>ccfftm</src> on all rows of the result, and then on all
// planes of that result.  See <src>scfftm</src> and <src>ccfftm</src> for
// more information about the algorithms used.
//
// </example>
// The following examples are for Origin series only.
//
// Example 1:  Initialize the TABLE array in preparation for doing a
// three-dimensional FFT of size 128 by 128 by 128.  In this case only
// the isign, n1, n2, n3, and table arguments are used; you can use dummy
// arguments or zeros for other arguments.
//
// <srcblock>
//       REAL TABLE ((15 + 128) + 2(15+128) + 2( 15 + 128))
//       CALL SCFFT3D (0, 128, 128, 128, 0.0, DUMMY, 1, 1, DUMMY, 1, 1,
//      &  TABLE, DUMMY, 0)
// </srcblock>
//
// Example 2:  X is a real array of size (0:128, 0:128, 0:128).  The
// first 128 elements of each dimension contain data; for performance
// reasons, the extra element forces the leading dimensions to be odd
// numbers.  Y is a complex array of dimension (0:64, 0:128, 0:128).
// Take the three-dimensional FFT of X and store it in Y.  Initialize the
// TABLE array, as in example 1.
//
// <srcblock>
//       REAL    X(0:128, 0:128, 0:128)
//       COMPLEX Y(0:64,  0:128, 0:128)
//       REAL TABLE ((15+128) + 2(15 + 128) + 2(15 + 128))
//       REAL    WORK(128*128*128)
//       ...
//       CALL SCFFT3D(0, 128, 128, 128, 1.0, X, 129, 129,
//      &    Y, 65, 129, TABLE, WORK, 0)
//       CALL SCFFT3D(1, 128, 128, 128, 1.0, X, 129, 129,
//      &    Y, 65, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 3:  With X and Y as in example 2, take the inverse FFT of Y
// and store it back in X.  The scale factor 1/(128**3) is used.  Assume
// that the TABLE array is initialized already.
//
// <srcblock>
//       CALL CSFFT3D(-1, 128, 128, 128, 1.0/128.0**3, Y, 65, 129,
//      &    X, 130, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 4:  Perform the same computation as in example 2, but assume
// that the lower bound of each array is 1, rather than 0.  No change is
// made in the subroutine calls.
//
// <srcblock>
//       REAL    X(129, 129, 129)
//       COMPLEX Y(65, 129, 129)
//       REAL TABLE ((15+128) + 2(15 + 128) + 2(15 + 128))
//       REAL    WORK(128*128*128)
//       ...
//       CALL SCFFT3D(0, 128, 128, 128, 1.0, X, 129, 129,
//      &    Y, 65, 129, TABLE, WORK, 0)
//       CALL SCFFT3D(1, 128, 128, 128, 1.0, X, 129, 129,
//      &    X, 129, 129, TABLE, WORK, 0)
// </srcblock>
//
// Example 5:  Perform the same computation as in example 4, but
// equivalence the input and output arrays to save storage space.  Assume
// that the TABLE array is initialized already.
//
// <srcblock>
//       REAL    X(130, 129, 129)
//       COMPLEX Y(65, 129, 129)
//       EQUIVALENCE (X(1, 1, 1), Y(1, 1, 1))
//       ...
//       CALL SCFFT3D(1, 128, 128, 128, 1.0, X, 130, 129,
//      &    Y, 65, 129, TABLE, WORK, 0)
// </srcblock>
// </example>
//
// Input parameters:
// <dl compact>
// <dt><b>isign</b>
// <dd>    Integer.
//         Specifies whether to initialize the table array or to do the
//         forward or inverse Fourier transform, as follows:
//
//         If isign = 0, the routine initializes the table array and
//         returns.  In this case, the only arguments used or checked
//         are isign, n1, n2, n3, and table.
//
//         If isign = +1 or -1, the value of isign is the sign of the
//         exponent used in the FFT formula.
//
// <dt><b>n1</b>
// <dd>    Integer.
//         Transform size in the first dimension.  If n1 is not
//         positive, <src>scfft3d</src> returns without computing a transform.
//
// <dt><b>n2</b>
// <dd>    Integer.
//         Transform size in the second dimension.  If n2 is not
//         positive, <src>scfft3d</src> returns without computing a transform.
//
// <dt><b>n3</b>
// <dd>    Integer.
//         Transform size in the third dimension.  If n3 is not
//         positive, <src>scfft3d</src> returns without computing a transform.
//
// <dt><b>scale</b>
// <dd>    Scale factor.
//         <src>scfft3d</src>: real.
//         <src>dzfft3d</src>: double precision.
//         <src>csfft3d</src>: real.
//         <src>zdfft3d</src>: double precision.
//         Each element of the output array is multiplied by scale
//         after taking the Fourier transform, as defined previously.
//
// <dt><b>x</b>
// <dd>    Array of dimension (0:ldx-1, 0:ldx2-1, 0:n3-1).
//         <src>scfft3d</src>: real array.
//         <src>dzfft3d</src>: double precision array.
//         <src>csfft3d</src>: complex array.
//         <src>zdfft3d</src>: double complex array.
//
//         Array of values to be transformed.
//
// <dt><b>ldx</b>
// <dd>    Integer.
//         The first dimension of x, as it was declared in the calling
//         program (the leading dimension of x).
//
//         <src>scfft3d, dzfft3d</src>:  ldx >= MAX(n1, 1).
//         <src>csfft3d, zdfft3d</src>:  ldx >= MAX(n1/2 + 1, 1).
//
// <dt><b>ldx2</b>
// <dd>    Integer.
//         The second dimension of x, as it was declared in the calling
//         program.  ldx2 >= MAX(n2, 1).
//
// <dt><b>ldy</b>
// <dd>    Integer.
//         The first dimension of y, as it was declared in the calling
//         program; that is, the leading dimension of y.
//
//         <src>scfft3d, dzfft3d</src>:  ldy >= MAX(n1/2 + 1, 1).
//         <src>csfft3d, zdfft3d</src>:  ldy >= MAX(n1 + 2, 1).
//
//         In the complex-to-real routine, two extra elements are in
//         the first dimension (that is, ldy >= n1 + 2, rather than
//         just ldy >= n1).  These elements are needed for intermediate
//         storage during the computation.  On exit, their value is
//         undefined.
//
// <dt><b>ldy2</b>
// <dd>    Integer.
//         The second dimension of y, as it was declared in the calling
//         program.  ldy2 >= MAX(n2, 1).
//
// <dt><b>isys</b>
// <dd>    Algorithm used; value dependent on hardware system.  Currently, no
//         special options are supported; therefore, you must always specify
//         an isys argument as constant 0.
//
//         isys = 0 or 1 depending on the amount of workspace the user
//         can provide to the routine.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>y</b>
// <dd>    Array of dimension (0:ldy-1, 0:ldy2-1, 0:n3-1).
//         <src>scfft3d</src>: complex array.
//         <src>dzfft3d</src>: double complex array.
//         <src>csfft3d</src>: real array.
//         <src>zdfft3d</src>: double precision array.
//
//         Output array of transformed values.  The output array can be
//         the same as the input array, in which case, the transform is
//         done in place; that is, the input array is overwritten with
//         the transformed values.  In this case, it is necessary that
//         the following equalities hold:
//
//         <src>scfft3d, dzfft3d</src>:  ldx = 2 * ldy, and ldx2 = ldy2.
//         <src>csfft3d, zdfft3d</src>:  ldy = 2 * ldx, and ldx2 = ldy2.
//
// <dt><b>table</b>
// <dd>    Real array; dimension (15 + n1) + 2(15 + n2) + 2(15 + n3).
//
//         Table of factors and trigonometric functions.
//
//         This array must be initialized by a call to <src>scfft3d</src> or
//         <src>csfft3d</src> with isign = 0.
//
//         If isign = 0, table is initialized to contain trigonometric
//         tables needed to compute a three-dimensional FFT of size n1
//         by n2 by n3.  If isign = +1 or -1, the values in table are
//         assumed to be initialized already by a prior call with isign
//         = 0.
//
// <dt><b>work</b>
// <dd>    Real array; dimension n1 * n2 * n3.
//
//         Work array.  This is a scratch array used for intermediate
//         calculations.  Its address space must be different from that
//         of the input and output arrays.
//
// </dl>
// <group>
static void scfft3d(Int isign, Int n1, Int n2, Int n3, Float scale,
		    Float* x, Int ldx, Int ldx2, Complex* y, Int ldy,
		    Int ldy2, Float* table, Float* work, Int isys); 
static void dzfft3d(Int isign, Int n1, Int n2, Int n3, Double scale,
		    Double* x, Int ldx, Int ldx2, DComplex* y, Int
		    ldy, Int ldy2, Double* table, Double* work, Int
		    isys); 
static void csfft3d(Int isign, Int n1, Int n2, Int n3, Float scale,
		    Complex* x, Int ldx, Int ldx2, Float* y, Int ldy,
		    Int ldy2, Float* table, Float* work, Int isys); 
static void zdfft3d(Int isign, Int n1, Int n2, Int n3, Double scale,
		    DComplex* x, Int ldx, Int ldx2, Double* y, Int
		    ldy, Int ldy2, Double* table, Double* work, Int
		    isys); 
// </group>
};

} //# NAMESPACE CASACORE - END

#endif
