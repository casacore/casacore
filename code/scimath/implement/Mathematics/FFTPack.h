//# extern_fft.h: C++ wrapper functions for FORTRAN FFT code
//# Copyright (C) 1993,1994,1995
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

// <summary> C++ wrapper functions for FORTRAN FFT code </summary>

// <synopsis>
// C++ wrapper functions for FORTRAN FFT code.
// </synopsis>

// <group name="FORTRAN FFT code C++ wrapper functions">

void scopy(int *, float *, int *, float *, int *);
void scopy(int *, double *, int *, double *, int *);

void sscal(int *, float *, float *, int *);
void sscal(int *, double *, double *, int *);

void cfftf(int *, float *, float *);
void cfftf(int *, double *, double *);

void cfftb(int *, float *, float *);
void cfftb(int *, double *, double *);

void cffti(int *, float *);
void cffti(int *, double *);

void rfftf(int *, float *, float *);
void rfftf(int *, double *, double *);

void rfftb(int *, float *, float *);
void rfftb(int *, double *, double *);

void rffti(int *, float *);
void rffti(int *, double *);


// R.C. Singleton functions. For documentation, consult <src>fftpak.f</src> .
// The arguments are the same as for the FORTRAN <src>mfft</src>
// and <src>mdfft</src> functions --- except that the temporary work arrays
// are handled here in <src>extern_fft.cc</src> .
// <group>

void mfft(float *re, float *im, int *ntot, int *n, int *nspan, int *isn);
void mfft(double *re, double *im, int *ntot, int *n, int *nspan, int *isn);

// </group>


// </group>

#endif
