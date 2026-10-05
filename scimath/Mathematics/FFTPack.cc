// # FFTPack.cc: C++ wrapper functions for Fortran FFTPACK code
// # Copyright (C) 1993,1994,1995,1997,1999,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/scimath/Mathematics/FFTPack.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

extern "C" {
void cffti_(int*, float*);
void cfft2i_(const int*, const int*, float*, const int*, int*);
void dcffti_(int*, double*);
void cfftf_(int*, float*, float*);
void dcfftf_(int*, double*, double*);
void cfftb_(int*, float*, float*);
void dcfftb_(int*, double*, double*);
void cfft2f_(const int*, const int*, const int*, Complex*, const float*, const int*, float*,
             const int*, int*);
void cfft2b_(const int*, const int*, const int*, Complex*, const float*, const int*, float*,
             const int*, int*);
}

extern "C" {
void rfftf_(int*, float*, float*);
void rfftb_(int*, float*, float*);
void rffti_(int*, float*);
void drfftf_(int*, double*, double*);
void drfftb_(int*, double*, double*);
void drffti_(int*, double*);
}

extern "C" {
void ezffti_(int*, float*);
void ezfftf_(int*, float*, float*, float*, float*, float*);
void ezfftb_(int* n, float*, float*, float*, float*, float*);
}

extern "C" {
void sinti_(int*, float*);
void dsinti_(int*, double*);
void sint_(int*, float*, float*);
void dsint_(int*, double*, double*);
}

extern "C" {
void costi_(int*, float*);
void dcosti_(int*, double*);
void cost_(int*, float*, float*);
void dcost_(int*, double*, double*);
}

extern "C" {
void sinqi_(int*, float*);
void dsinqi_(int*, double*);
void sinqf_(int*, float*, float*);
void dsinqf_(int*, double*, double*);
void sinqb_(int*, float*, float*);
void dsinqb_(int*, double*, double*);
}

extern "C" {
void cosqi_(int*, float*);
void dcosqi_(int*, double*);
void cosqf_(int*, float*, float*);
void dcosqf_(int*, double*, double*);
void cosqb_(int*, float*, float*);
void dcosqb_(int*, double*, double*);
}

void FFTPack::cffti(int n, float* work) { cffti_((int*)&n, (float*)work); }

void FFTPack::cffti(int n, double* work) { dcffti_((int*)&n, (double*)work); }

void FFTPack::cfftf(int n, Complex* rdata, float* work) {
  DebugAssert(sizeof(Complex) == 2 * sizeof(float), AipsError);
  cfftf_((int*)&n, (float*)rdata, (float*)work);
}

void FFTPack::cfftf(int n, DComplex* rdata, double* work) {
  DebugAssert(sizeof(DComplex) == 2 * sizeof(double), AipsError);
  dcfftf_((int*)&n, (double*)rdata, (double*)work);
}

void FFTPack::cfftb(int n, Complex* rdata, float* work) {
  DebugAssert(sizeof(Complex) == 2 * sizeof(float), AipsError);
  cfftb_((int*)&n, (float*)rdata, (float*)work);
}

void FFTPack::cfftb(int n, DComplex* rdata, double* work) {
  DebugAssert(sizeof(DComplex) == 2 * sizeof(double), AipsError);
  dcfftb_((int*)&n, (double*)rdata, (double*)work);
}

void FFTPack::cfft2i(const int& n, const int& m, float*& wsave, const int& lensav, int& ier) {
  cfft2i_(&n, &m, wsave, &lensav, &ier);
}

void FFTPack::cfft2f(const int& ldim, const int& l, const int& m, Complex*& c, float*& wsave,
                     const int& lensav, float*& work, const int& lenwrk, int& ier) {
  cfft2f_(&ldim, &l, &m, c, wsave, &lensav, work, &lenwrk, &ier);
}
void FFTPack::cfft2b(const int& ldim, const int& l, const int& m, Complex*& c, float*& wsave,
                     const int& lensav, float*& work, const int& lenwrk, int& ier) {
  cfft2b_(&ldim, &l, &m, c, wsave, &lensav, work, &lenwrk, &ier);
}

void FFTPack::rffti(int n, float* work) { rffti_((int*)&n, (float*)work); }

void FFTPack::rffti(int n, double* work) { drffti_((int*)&n, (double*)work); }

void FFTPack::rfftf(int n, float* rdata, float* work) {
  rfftf_((int*)&n, (float*)rdata, (float*)work);
}

void FFTPack::rfftf(int n, double* rdata, double* work) {
  drfftf_((int*)&n, (double*)rdata, (double*)work);
}

void FFTPack::rfftb(int n, float* rdata, float* work) {
  rfftb_((int*)&n, (float*)rdata, (float*)work);
}

void FFTPack::rfftb(int n, double* rdata, double* work) {
  drfftb_((int*)&n, (double*)rdata, (double*)work);
}

void FFTPack::ezffti(int n, float* wsave) { ezffti_((int*)&n, (float*)wsave); }

void FFTPack::ezfftf(int n, float* r, float* azero, float* a, float* b, float* wsave) {
  ezfftf_((int*)&n, (float*)r, (float*)azero, (float*)a, (float*)b, (float*)wsave);
}

void FFTPack::ezfftb(int n, float* r, float* azero, float* a, float* b, float* wsave) {
  ezfftb_((int*)&n, (float*)r, (float*)azero, (float*)a, (float*)b, (float*)wsave);
}

void FFTPack::sinti(int n, float* wsave) { sinti_((int*)&n, (float*)wsave); }

void FFTPack::sinti(int n, double* wsave) { dsinti_((int*)&n, (double*)wsave); }

void FFTPack::sint(int n, float* x, float* wsave) { sint_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::sint(int n, double* x, double* wsave) {
  dsint_((int*)&n, (double*)x, (double*)wsave);
}

void FFTPack::costi(int n, float* wsave) { costi_((int*)&n, (float*)wsave); }

void FFTPack::costi(int n, double* wsave) { dcosti_((int*)&n, (double*)wsave); }

void FFTPack::cost(int n, float* x, float* wsave) { cost_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::cost(int n, double* x, double* wsave) {
  dcost_((int*)&n, (double*)x, (double*)wsave);
}

void FFTPack::sinqi(int n, float* wsave) { sinqi_((int*)&n, (float*)wsave); }

void FFTPack::sinqi(int n, double* wsave) { dsinqi_((int*)&n, (double*)wsave); }

void FFTPack::sinqf(int n, float* x, float* wsave) { sinqf_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::sinqf(int n, double* x, double* wsave) {
  dsinqf_((int*)&n, (double*)x, (double*)wsave);
}

void FFTPack::sinqb(int n, float* x, float* wsave) { sinqb_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::sinqb(int n, double* x, double* wsave) {
  dsinqb_((int*)&n, (double*)x, (double*)wsave);
}

void FFTPack::cosqi(int n, float* wsave) { cosqi_((int*)&n, (float*)wsave); }

void FFTPack::cosqi(int n, double* wsave) { dcosqi_((int*)&n, (double*)wsave); }

void FFTPack::cosqf(int n, float* x, float* wsave) { cosqf_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::cosqf(int n, double* x, double* wsave) {
  dcosqf_((int*)&n, (double*)x, (double*)wsave);
}

void FFTPack::cosqb(int n, float* x, float* wsave) { cosqb_((int*)&n, (float*)x, (float*)wsave); }

void FFTPack::cosqb(int n, double* x, double* wsave) {
  dcosqb_((int*)&n, (double*)x, (double*)wsave);
}
// Local Variables:
// compile-command: "gmake OPTLIB=1 FFTPack"
// End:

}  // namespace casacore
