//# extern_fft.cc: C++ wrapper functions for FORTRAN FFT code
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

#include <aips/Mathematics/extern_fft.h>

extern "C" {
  void cffti_(int *, float *);
  void dcffti_(int *, double *);
  void cfftf_(int *, float *, float *);
  void dcfftf_(int *, double *, double *);
  void cfftb_(int *, float *, float *);
  void dcfftb_(int *, double *, double *);
}

extern "C" {
  void rfftf_(int *, float *, float *);
  void rfftb_(int *, float *, float *);
  void rffti_(int *, float *);
  void drfftf_(int *, double *, double *);
  void drfftb_(int *, double *, double *);
  void drffti_(int *, double *);
}

extern "C" {
  void ezffti_(int *, float *);
  void ezfftf_(int *, float *, float *, float *, float *, float *);
  void ezfftb_(int * n, float *, float *, float *, float *, float *);
}

extern "C" {
  void sinti_(int *, float *);
  void dsinti_(int *, double *);
  void sint_(int *, float *, float *);
  void dsint_(int *, double *, double *);
}

extern "C" {
  void costi_(int *, float *);
  void dcosti_(int *, double *);
  void cost_(int *, float *, float *);
  void dcost_(int *, double *, double *);
}

extern "C" {
  void sinqi_(int *, float *);
  void dsinqi_(int *, double *);
  void sinqf_(int *, float *, float *);
  void dsinqf_(int *, double *, double *);
  void sinqb_(int *, float *, float *);
  void dsinqb_(int *, double *, double *);
}

extern "C" {
  void cosqi_(int *, float *);
  void dcosqi_(int *, double *);
  void cosqf_(int *, float *, float *);
  void dcosqf_(int *, double *, double *);
  void cosqb_(int *, float *, float *);
  void dcosqb_(int *, double *, double *);
}

void cffti(Int n, Float * work) {
  cffti_((int *) &n, (float *) work);
}

void cffti(Int n, Double * work) {
  dcffti_((int *) &n, (double *) work);
}

void  cfftf(Int n, Complex * rdata, Float * work) {
  cfftf_((int *) &n, (float *) rdata, (float *) work);
}

void cfftf(Int n, DComplex * rdata, Double * work) {
  dcfftf_((int *) &n, (double *) rdata, (double *) work);
}

void cfftb(Int n, Complex * rdata, Float * work) {
  cfftb_((int *) &n, (float *) rdata, (float *) work);
}

void cfftb(Int n, DComplex * rdata, Double * work) {
  dcfftb_((int *) &n, (double *) rdata, (double *) work);
}

void rffti(Int n, Float * work) {
  rffti_((int *) &n, (float *) work);
}

void rffti(Int n, Double * work) {
  drffti_((int *) &n, (double *) work);
}

void rfftf(Int n, Float * rdata, Float * work) {
  rfftf_((int *) &n, (float *) rdata, (float *) work);
}

void rfftf(Int n, Double * rdata, Double * work) {
  drfftf_((int *) &n, (double *) rdata, (double *) work);
}

void rfftb(Int n, Float * rdata, Float * work) {
  rfftb_((int *) &n, (float *) rdata, (float *) work);
}

void rfftb(Int n, Double * rdata, Double * work) {
  drfftb_((int *) &n, (double *) rdata, (double *) work);
}

void ezffti(Int n, Float * wsave) {
  ezffti_((int *) &n, (float *) wsave);
}

void ezfftf(Int n, Float * r, Float * azero, Float * a, Float * b, 
	    Float * wsave) {
  ezfftf_((int *) &n, (float *) r, (float *) azero, (float *) a, (float *) b,
	  (float *) wsave);
}

void ezfftb(Int n, Float * r, Float * azero, Float * a, Float * b, 
	    Float * wsave) {
  ezfftb_((int *) &n, (float *) r, (float *) azero, (float *) a, (float *) b,
	  (float *) wsave);
}

void sinti(Int n, Float * wsave) {
  sinti_((int *) &n, (float *) wsave);
}

void sinti(Int n, Double * wsave) {
  dsinti_((int *) &n, (double *) wsave);
}

void sint(Int n, Float * x, Float * wsave) {
  sint_((int *) &n, (float *) x, (float *) wsave);
}

void sint(Int n, Double * x, Double * wsave) {
   dsint_((int *) &n, (double *) x, (double *) wsave);
}

void costi(Int n, Float * wsave) {
  costi_((int *) &n, (float *) wsave);
}

void costi(Int n, Double * wsave) {
  dcosti_((int *) &n, (double *) wsave);
}

void cost(Int n, Float * x, Float * wsave) {
  cost_((int *) &n, (float *) x, (float *) wsave);
}

void cost(Int n, Double * x, Double * wsave) {
  dcost_((int *) &n, (double *) x, (double *) wsave);
}

void sinqi(Int n, Float * wsave) {
  sinqi_((int *) &n, (float *) wsave);
}

void sinqi(Int n, Double * wsave) {
  dsinqi_((int *) &n, (double *) wsave);
}

void sinqf(Int n, Float * x, Float * wsave) {
  sinqf_((int *) &n, (float *) x, (float *) wsave);
}

void sinqf(Int n, Double * x, Double * wsave) {
  dsinqf_((int *) &n, (double *) x, (double *) wsave);
}

void sinqb(Int n, Float * x, Float * wsave) {
  sinqb_((int *) &n, (float *) x, (float *) wsave);
}

void sinqb(Int n, Double * x, Double * wsave) {
  dsinqb_((int *) &n, (double *) x, (double *) wsave);
}

void cosqi(Int n, Float * wsave) {
  cosqi_((int *) &n, (float *) wsave);
}

void cosqi(Int n, Double * wsave) {
  dcosqi_((int *) &n, (double *) wsave);
}

void cosqf(Int n, Float * x, Float * wsave) {
  cosqf_((int *) &n, (float *) x, (float *) wsave);
}

void cosqf(Int n, Double * x, Double * wsave) {
  dcosqf_((int *) &n, (double *) x, (double *) wsave);
}

void cosqb(Int n, Float * x, Float * wsave) {
  cosqb_((int *) &n, (float *) x, (float *) wsave);
}

void cosqb(Int n, Double * x, Double * wsave) {
  dcosqb_((int *) &n, (double *) x, (double *) wsave);
}
