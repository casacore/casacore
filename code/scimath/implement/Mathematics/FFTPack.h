//# extern_fft.h: C++ wrapper functions for FORTRAN FFT code
//# Copyright (C) 1993,1994,1995,1997
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

#if !defined(AIPS_EXTERN_FFT_H)
#define AIPS_EXTERN_FFT_H

#if defined(_AIX)
#pragma implementation ("extern_fft.cc")
#endif

#include <aips/aips.h>

// <summary>Complex to complex transforms</summary>
// <synopsis>
// These are C++ wrapper functions for the FORTRAN complex to complex transform
// routines in the files fftpak.f and dfftpack.f. The purpose of these
// definitions is to overload the functions so that C++ users can access the
// functions in either fftpak or dfftpack with an identical function
// interface.

// These routines only do one dimensional transforms with the first element of
// the array being the "origin" of the transform. The
// <linkto class="FFTServer">FFTServer</linkto> class uses these functions to
// implement multi-dimensional transforms with the origin of the transform
// either at the centre or the first element of the Array.

// Before using the forward transform <src>cfftf</src> or the backward
// transform src>cfftb</src> the work array <src>wsave</src> must be
// initialised using the <src>cffti</src> function.
// </synopsis>

// <group name="complex-to-complex-fft">

// cffti initializes the array wsave which is used in both cfftf and cfftb. The
// prime factorization of n together with a tabulation of the trigonometric
// functions are computed and stored in wsave.
// 
// Input parameter:
// <dl compact>
// <dt><b>n</b>
// <dd>       the length of the sequence to be transformed
// </dl>
// Output parameter:
// <dl compact>
// <dt><b>wsave</b>
// <dd>    a work array which must be dimensioned at least 4*n+15
//         the same work array can be used for both cfftf and cfftb
//         as long as n remains unchanged. different wsave arrays
//         are required for different values of n. the contents of
//         wsave must not be changed between calls of cfftf or cfftb.
// </dl>
// <group>
void cffti(Int n, Float * wsave);
void cffti(Int n, Double * wsave);
// </group>

// cfftf computes the forward complex discrete Fourier
// transform (the Fourier analysis). Equivalently, cfftf computes
// the Fourier coefficients of a complex periodic sequence.
// the transform is defined below at output parameter c.
// 
// The transform is not normalized. To obtain a normalized transform
// the output must be divided by n. Otherwise a call of cfftf
// followed by a call of cfftb will multiply the sequence by n.
// 
// The array wsave which is used by subroutine cfftf must be
// initialized by calling subroutine <src>cffti(n,wsave)</src>.
// 
// Input parameters:
// <dl compact>
// <dt><b>n</b>
// <dd>   the length of the complex sequence c. the method is
//        more efficient when n is the product of small primes. n
// <dt><b>c</b>
// <dd>    a complex array of length n which contains the sequence
// <dt><b>wsave</b>
// <dd>    a real work array which must be dimensioned at least 4n+15
//         in the program that calls cfftf. the wsave array must be
//         initialized by calling subroutine cffti(n,wsave) and a
//         different wsave array must be used for each different
//         value of n. this initialization does not have to be
//         repeated so long as n remains unchanged thus subsequent
//         transforms can be obtained faster than the first.
//         the same wsave array can be used by cfftf and cfftb.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>c</b>
// <dd>   for j=1,...,n<br>
//            c(j)=the sum from k=1,...,n of<br>
//                  c(k)*exp(-i*(j-1)*(k-1)*2*pi/n)<br>
//                        where i=sqrt(-1)<br>
// <dt><b>wsave</b>
// <dd>    contains initialization calculations which must not be
//         destroyed between calls of subroutine cfftf or cfftb
// </dl>
// <group>
void cfftf(Int n, Float * c, Float * wsave);
void cfftf(Int n, Double * c, Double * wsave);
// </group>

// cfftb computes the backward complex discrete Fourier
// transform (the Fourier synthesis). Equivalently , cfftb computes
// a complex periodic sequence from its Fourier coefficients.
// The transform is defined below with output parameter c.
// 
// A call of cfftf followed by a call of cfftb will multiply the
// sequence by n.
// 
// The array wsave which is used by subroutine cfftb must be
// initialized by calling <src>cffti(n,wsave)</src>.
// 
// Input parameters:
// <dl compact>
// <dt><b>n</b>
// <dd>          The length of the complex sequence c. the method is
//               more efficient when n is the product of small primes.
// <dt><b>c</b>
// <dd>          A complex array of length n which contains the sequence
// <dt><b>wsave</b> 
// <dd>          A real work array which must be dimensioned at least 4n+15
//               in the program that calls cfftb. the wsave array must be
//               initialized by calling subroutine cffti(n,wsave) and a
//               different wsave array must be used for each different
//               value of n. this initialization does not have to be
//               repeated so long as n remains unchanged thus subsequent
//               transforms can be obtained faster than the first.
//               the same wsave array can be used by cfftf and cfftb.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>c</b>     
// <dd>          for j=1,...,n<br>
//                 c(j)=the sum from k=1,...,n of<br>
//                     c(k)*exp(i*(j-1)*(k-1)*2*pi/n)<br>

// <dt><b>wsave</b>
// <dd>          contains initialization calculations which must not be
//               destroyed between calls of subroutine cfftf or cfftb
// </dl>
// <group>
void cfftb(Int n, Float * c, Float * wsave);
void cfftb(Int n, Double * c, Double * wsave);
// </group>
// </group>

// <summary>Real to complex & complex to real transforms</summary>
// <synopsis>
// These are C++ wrapper functions for the FORTRAN real to complex & complex to
// real transform routines in the files fftpak.f and dfftpack.f. The purpose of
// these definitions is to overload the functions so that C++ users can access
// the functions in either fftpak or dfftpack with an identical function
// interface.

// These routines only do one dimensional transforms with the first element of
// the array being the "origin" of the transform. The
// <linkto class="FFTServer">FFTServer</linkto> class uses these functions to
// implement multi-dimensional transforms with the origin of the transform
// either at the centre or the first element of the Array.

// Before using the forward transform <src>rfftf</src> or the backward
// transform src>rfftb</src> the work array <src>wsave</src> must be
// initialised using the <src>rffti</src> function.

// The "ez" routines provide a simpler interface to the real to complex
// transforms. They do not destroy the input arrays and are available in single
// precision only. Internally they use use the "rfft" functions.
// </synopsis>

// <group name="real-to-complex-fft">

// rffti initializes the array wsave which is used in both <src>rfftf</src> and
// <src>rfftb</src>. The prime factorization of n together with a tabulation of
// the trigonometric functions are computed and stored in wsave.
//
// Input parameter:
// <dl compact>
// <dt><b>n</b>
// <dd>       the length of the sequence to be transformed.
// </dl>
// Output parameter:
// <dl compact>
// <dt><b>wsave</b>
// <dd>    a work array which must be dimensioned at least 2*n+15.
//         the same work array can be used for both rfftf and rfftb
//         as long as n remains unchanged. different wsave arrays
//         are required for different values of n. the contents of
//         wsave must not be changed between calls of rfftf or rfftb.
// </dl>
// <group>
void rffti(Int n, Float * wsave);
void rffti(Int n, Double * wsave);
// </group>

// rfftf computes the Fourier coefficients of a real perodic sequence (Fourier
// analysis). The transform is defined below at output parameter r.
// 
// Input parameters:
// <dl compact>
// <dt><b>n</b>
// <dd>    the length of the array r to be transformed.  The method
//         is most efficient when n is a product of small primes.
//         n may change so long as different work arrays are provided
// <dt><b>r</b>
// <dd>    a real array of length n which contains the sequence
//         to be transformed
// <dt><b>wsave</b>
// <dd>    a work array which must be dimensioned at least 2*n+15
//         in the program that calls rfftf. The wsave array must be
//         initialized by calling subroutine <src>rffti(n,wsave)</src> and a
//         different wsave array must be used for each different
//         value of n. This initialization does not have to be
//         repeated so long as n remains unchanged thus subsequent
//         transforms can be obtained faster than the first as
//         the same wsave array can be used by rfftf and rfftb.
// </dl>
// output parameters
// <dl compact>
// <dt><b>r</b>
// <dd>    r(1) = the sum from i=1 to i=n of r(i)<br>
//         if n is even set l = n/2   , if n is odd set l = (n+1)/2<br>
//         then for k = 2,...,l<br>
//              r(2*k-2) = the sum from i = 1 to i = n of<br>
//                         r(i)*cos((k-1)*(i-1)*2*pi/n)<br>
//              r(2*k-1) = the sum from i = 1 to i = n of<br>
//                         -r(i)*sin((k-1)*(i-1)*2*pi/n)<br>
//         if n is even<br>
//              r(n) = the sum from i = 1 to i = n of<br>
//                   (-1)**(i-1)*r(i)<br>
// 
//         note:
//              this transform is unnormalized since a call of rfftf
//              followed by a call of rfftb will multiply the input
//              sequence by n.
// <dt><b>wsave</b>
// <dd>    contains results which must not be destroyed between
//         calls of rfftf or rfftb.
// </dl>
// <group>
void rfftf(Int n, Float * r, Float * wsave);
void rfftf(Int n, Double * r, Double * wsave);
// </group>


// rfftb computes the real perodic sequence from its Fourier coefficients
// (Fourier synthesis). The transform is defined below at output parameter r.
// 
// Input parameters:
// <dl compact>
// <dt><b>n</b>
// <dd>    the length of the array r to be transformed.  the method
//         is most efficient when n is a product of small primes.
//         n may change so long as different work arrays are provided
// <dt><b>r</b>
// <dd>    a real array of length n which contains the sequence
//         to be transformed
// <dt><b>wsave</b>
// <dd>    a work array which must be dimensioned at least 2*n+15.
//         in the program that calls rfftb. the wsave array must be
//         initialized by calling subroutine rffti(n,wsave) and a
//         different wsave array must be used for each different
//         value of n. this initialization does not have to be
//         repeated so long as n remains unchanged thus subsequent
//         transforms can be obtained faster than the first.
//         the same wsave array can be used by rfftf and rfftb.
// </dl>
// Output parameters:
// <dl compact>
// <dt><b>r</b>
// <dd>    for n even and for i = 1,...,n<br>
//              r(i) = r(1)+(-1)**(i-1)*r(n)<br>
//                   plus the sum from k=2 to k=n/2 of<br>
//                    2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)<br>
//                   -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)<br>
//         for n odd and for i = 1,...,n<br>
//              r(i) = r(1) plus the sum from k=2 to k=(n+1)/2 of<br>
//                   2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)<br>
//                  -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)<br>
// 
//         note:
//              this transform is unnormalized since a call of rfftf
//              followed by a call of rfftb will multiply the input
//              sequence by n.
// <dt><b>wsave</b>
// <dd>    contains results which must not be destroyed between
//         calls of rfftb or rfftf.
// </dl>
// <group>
void rfftb(Int n, Float * r, Float * wsave);
void rfftb(Int n, Double * r, Double * wsave);
// </group>


// <group>
void ezffti(Int n, Float * wsave);
void ezfftf(Int n, Float * r, Float * azero, Float * a, Float * b, 
	    Float * wsave);
void ezfftb(Int n, Float * r, Float * azero, Float * a, Float * b, 
	    Float * wsave);
// </group>
// </group>

// <summary>Sine transform</summary>
// <group name="sine-transform">
// <group>
void sinti(Int n, Float * wsave);
void sinti(Int n, Double * wsave);
// </group>
// <group>
void sint(Int n, Float * x, Float * wsave);
void sint(Int n, Double * x, Double * wsave);
// </group>
// </group>

// <summary>Cosine transform</summary>
// <group name="cosine-transform">
// <group>
void costi(Int n, Float * wsave);
void costi(Int n, Double * wsave);
// </group>
// <group>
void cost(Int n, Float * x, Float * wsave);
void cost(Int n, Double * x, Double * wsave);
// </group>
// </group>

// <summary>Not quite sure just yet</summary>
// <group name="sine-fft">
// <group>
void sinqi(Int n, Float * wsave);
void sinqi(Int n, Double * wsave);
// </group>
// <group>
void sinqf(Int n, Float * x, Float * wsave);
void sinqf(Int n, Double * x, Double * wsave);
// </group>
// <group>
void sinqb(Int n, Float * x, Float * wsave);
void sinqb(Int n, Double * x, Double * wsave);
// </group>
// </group>

// <summary>Not quite sure just yet</summary>
// <group name="cosine-fft">
// <group>
void cosqi(Int n, Float * wsave);
void cosqi(Int n, Double * wsave);
// <group>
// </group>
void cosqf(Int n, Float * x, Float * wsave);
void cosqf(Int n, Double * x, Double * wsave);
// </group>
// <group>
void cosqb(Int n, Float * x, Float * wsave);
void cosqb(Int n, Double * x, Double * wsave);
// </group>
// </group>
#endif
