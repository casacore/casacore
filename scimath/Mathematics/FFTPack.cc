//# FFTPack.cc: C++ wrapper functions for Fortran FFTPACK code
//# Copyright (C) 1993,1994,1995,1997,1999,2001
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

#include <casacore/scimath/Mathematics/FFTPack.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

extern "C" {
  void cffti_(int*, float*);
  void cfft2i_(const Int*, const Int*, Float*, const Int*, Int*);
  void dcffti_(int*, double*);
  void cfftf_(int*, float*, float*);
  void dcfftf_(int*, double*, double*);
  void cfftb_(int*, float*, float*);
  void dcfftb_(int*, double*, double*);
  void cfft2f_(const Int*, const Int*, const Int*, Complex*, const Float*, const Int*, Float* , const Int*, Int*);
  void cfft2b_(const Int*, const Int*, const Int*, Complex*, const Float*, const Int*, Float* , const Int*, Int*);
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

void FFTPack::cffti(Int n, Float* work) {
  cffti_((int*) &n, (float*) work);
}

void FFTPack::cffti(Int n, Double* work) {
  dcffti_((int*) &n, (double*) work);
}

void FFTPack::cfftf(Int n, Complex* rdata, Float* work) {
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  cfftf_((int*) &n, (float*) rdata, (float*) work);
}

void FFTPack::cfftf(Int n, DComplex* rdata, Double* work) {
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dcfftf_((int*) &n, (double*) rdata, (double*) work);
}

void FFTPack::cfftb(Int n, Complex* rdata, Float* work) {
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  cfftb_((int*) &n, (float*) rdata, (float*) work);
}

void FFTPack::cfftb(Int n, DComplex* rdata, Double* work) {
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dcfftb_((int*) &n, (double*) rdata, (double*) work);
}


  void FFTPack::cfft2i(const Int& n, const Int& m, Float *& wsave, const Int& lensav, Int& ier){
    cfft2i_(&n, &m, wsave, &lensav, &ier);
  }

  void FFTPack::cfft2f (const Int& ldim, const Int& l, const Int& m, Complex*& c, Float*& wsave, const Int& lensav,
			                     Float *& work, const Int& lenwrk, Int& ier){
    cfft2f_(&ldim, &l, &m, c, wsave, &lensav, work, &lenwrk, &ier); 

  }
  void FFTPack::cfft2b (const Int& ldim, const Int& l, const Int& m, Complex* & c, Float *& wsave, const Int& lensav,
			Float*& work, const Int& lenwrk, Int& ier){
    cfft2b_(&ldim, &l, &m, c, wsave, &lensav, work, &lenwrk, &ier);

  }

void FFTPack::rffti(Int n, Float* work) {
  rffti_((int*) &n, (float*) work);
}

void FFTPack::rffti(Int n, Double* work) {
  drffti_((int*) &n, (double*) work);
}

void FFTPack::rfftf(Int n, Float* rdata, Float* work) {
  rfftf_((int*) &n, (float*) rdata, (float*) work);
}

void FFTPack::rfftf(Int n, Double* rdata, Double* work) {
  drfftf_((int*) &n, (double*) rdata, (double*) work);
}

void FFTPack::rfftb(Int n, Float* rdata, Float* work) {
  rfftb_((int*) &n, (float*) rdata, (float*) work);
}

void FFTPack::rfftb(Int n, Double* rdata, Double* work) {
  drfftb_((int*) &n, (double*) rdata, (double*) work);
}

void FFTPack::ezffti(Int n, Float* wsave) {
  ezffti_((int*) &n, (float*) wsave);
}

void FFTPack::ezfftf(Int n, Float* r, Float* azero, Float* a, Float* b, 
	    Float* wsave) {
  ezfftf_((int*) &n, (float*) r, (float*) azero, (float*) a, (float*) b,
	  (float*) wsave);
}

void FFTPack::ezfftb(Int n, Float* r, Float* azero, Float* a, Float* b, 
	    Float* wsave) {
  ezfftb_((int*) &n, (float*) r, (float*) azero, (float*) a, (float*) b,
	  (float*) wsave);
}

void FFTPack::sinti(Int n, Float* wsave) {
  sinti_((int*) &n, (float*) wsave);
}

void FFTPack::sinti(Int n, Double* wsave) {
  dsinti_((int*) &n, (double*) wsave);
}

void FFTPack::sint(Int n, Float* x, Float* wsave) {
  sint_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::sint(Int n, Double* x, Double* wsave) {
   dsint_((int*) &n, (double*) x, (double*) wsave);
}

void FFTPack::costi(Int n, Float* wsave) {
  costi_((int*) &n, (float*) wsave);
}

void FFTPack::costi(Int n, Double* wsave) {
  dcosti_((int*) &n, (double*) wsave);
}

void FFTPack::cost(Int n, Float* x, Float* wsave) {
  cost_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::cost(Int n, Double* x, Double* wsave) {
  dcost_((int*) &n, (double*) x, (double*) wsave);
}

void FFTPack::sinqi(Int n, Float* wsave) {
  sinqi_((int*) &n, (float*) wsave);
}

void FFTPack::sinqi(Int n, Double* wsave) {
  dsinqi_((int*) &n, (double*) wsave);
}

void FFTPack::sinqf(Int n, Float* x, Float* wsave) {
  sinqf_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::sinqf(Int n, Double* x, Double* wsave) {
  dsinqf_((int*) &n, (double*) x, (double*) wsave);
}

void FFTPack::sinqb(Int n, Float* x, Float* wsave) {
  sinqb_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::sinqb(Int n, Double* x, Double* wsave) {
  dsinqb_((int*) &n, (double*) x, (double*) wsave);
}

void FFTPack::cosqi(Int n, Float* wsave) {
  cosqi_((int*) &n, (float*) wsave);
}

void FFTPack::cosqi(Int n, Double* wsave) {
  dcosqi_((int*) &n, (double*) wsave);
}

void FFTPack::cosqf(Int n, Float* x, Float* wsave) {
  cosqf_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::cosqf(Int n, Double* x, Double* wsave) {
  dcosqf_((int*) &n, (double*) x, (double*) wsave);
}

void FFTPack::cosqb(Int n, Float* x, Float* wsave) {
  cosqb_((int*) &n, (float*) x, (float*) wsave);
}

void FFTPack::cosqb(Int n, Double* x, Double* wsave) {
  dcosqb_((int*) &n, (double*) x, (double*) wsave);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 FFTPack"
// End: 

} //# NAMESPACE CASACORE - END

