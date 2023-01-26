//# extern_fft.cc: C++ wrapper functions for FORTRAN FFT code
//# Copyright (C) 1993,1994,1995,1997,1999
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

#include <casacore/scimath/Mathematics/SCSL.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/scimath/Mathematics.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#if ! defined(HAVE_SCSL)
#define PN(a)
#else
#define PN(a) a
extern "C" {
  void ccfft_(int*, int*, float*, float*, float*, float*, float*,
	      int*);
  void zzfft_(int*, int*, double*, double*, double*, double*,
	      double*, int*);
}

extern "C" {
  void scfft_(int*, int*, float*, float*, float*, float*, float*,
	      int*); 
  void dzfft_(int*, int*, double*, double*, double*, double*,
	      double*, int*);
  void csfft_(int*, int*, float*, float*, float*, float*, float*,
	      int*);
  void zdfft_(int*, int*, double*, double*, double*, double*,
	      double*, int*); 
}

extern "C" {
  void ccfftm_(int*, int*, int*, float*, float*, int*, float*,
	       int*, float*, float*, int*); 
  void zzfftm_(int*, int*, int*, double*, double*, int*, double*,
	       int*, double*, double*, int*);
}

extern "C" {
  void scfftm_(int*, int*, int*, float*, float*, int*, float*,
	       int*, float*, float*, int*); 
  void dzfftm_(int*, int*, int*, double*, double*, int*, double*,
	       int*, double*, double*, int*);
  void csfftm_(int*, int*, int*, float*, float*, int*, float*,
	       int*, float*, float*, int*); 
  void zdfftm_(int*, int*, int*, double*, double*, int*, double*,
	       int*, double*, double*, int*);
}

extern "C" {
  void ccfft2d_(int*, int*, int*, float*, float*, int*, float*,
		int*, float*, float*, int*); 
  void zzfft2d_(int*, int*, int*, double*, double*, int*, double*,
		int*, double*, double*, int*);
}

extern "C" {
  void scfft2d_(int*, int*, int*, float*, float*, int*, float*,
		int*, float*, float*, int*); 
  void dzfft2d_(int*, int*, int*, double*, double*, int*, double*,
		int*, double*, double*, int*);
  void csfft2d_(int*, int*, int*, float*, float*, int*, float*,
		int*, float*, float*, int*); 
  void zdfft2d_(int*, int*, int*, double*, double*, int*, double*,
		int*, double*, double*, int*);
}

extern "C" {
  void ccfft3d_(int*, int*, int*, int*, float*, float*, int*,
		int*, float*, int*, int*, float*, float*, int*); 
  void zzfft3d_(int*, int*, int*, int*, double*, double*, int*,
		int*, double*, int*, int*, double*, double*, int*);
}

extern "C" {
  void scfft3d_(int*, int*, int*, int*, float*, float*, int*,
		int*, float*, int*, int*, float*, float*, int*); 
  void dzfft3d_(int*, int*, int*, int*, double*, double*, int*,
		int*, double*, int*, int*, double*, double*, int*); 
  void csfft3d_(int*, int*, int*, int*, float*, float*, int*,
		int*, float*, int*, int*, float*, float*, int*); 
  void zdfft3d_(int*, int*, int*, int*, double*, double*, int*,
		int*, double*, int*, int*, double*, double*, int*); 
}
#endif

void SCSL::ccfft(int32_t PN(isign), int32_t PN(n), float PN(scale), Complex* PN(x),
	   Complex* PN(y), float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  ccfft_((int*) &isign, (int*) &n, (float*) &scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::ccfft(int32_t PN(isign), int32_t PN(n), double PN(scale), DComplex* PN(x),
	   DComplex* PN(y), double* PN(table), double* PN(work),
	   int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  zzfft_((int*) &isign, (int*) &n, (double*) &scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*)
	 &isys); 
#endif
}

void SCSL::scfft(int32_t PN(isign), int32_t PN(n), float PN(scale), float* PN(x),
           Complex* PN(y), float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  scfft_((int*) &isign, (int*) &n, (float*) &scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::scfft(int32_t PN(isign), int32_t PN(n), double PN(scale), double* PN(x),
	   DComplex* PN(y), double* PN(table), double* PN(work),
	   int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfft_((int*) &isign, (int*) &n, (double*) &scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*)
	 &isys); 
#endif
}

void SCSL::csfft(int32_t PN(isign), int32_t PN(n), float PN(scale), Complex* PN(x),
	   float* PN(y), float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  csfft_((int*) &isign, (int*) &n, (float*) &scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::csfft(int32_t PN(isign), int32_t PN(n), double PN(scale), DComplex* PN(x),
	   double* PN(y), double* PN(table), double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  zdfft_((int*) &isign, (int*) &n, (double*) &scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::ccfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), float PN(scale),
	    Complex* PN(x), int32_t PN(ldx), Complex* PN(y), int32_t PN(ldy),
	    float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  ccfftm_((int*) &isign, (int*) &n, (int*) &lot, (float*) &scale,
	  (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	  table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::zzfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), double PN(scale),
	    DComplex* PN(x), int32_t PN(ldx), DComplex* PN(y), int32_t PN(ldy),
	    double* PN(table), double* PN(work), int32_t PN(isys)) {
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  zzfftm_((int*) &isign, (int*) &n, (int*) &lot, (double*) &scale,
	  (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	  table, (double*) work, (int*) &isys);
#endif
}

void SCSL::scfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), float PN(scale),
	    float* PN(x), int32_t PN(ldx), Complex* PN(y), int32_t PN(ldy),
	    float* PN(table), float* PN(work), int32_t PN(isys)) {
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  scfftm_((int*) &isign, (int*) &n, (int*) &lot, (float*) &scale,
	  (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	  table, (float*) work, (int*) &isys);
#endif
}

void SCSL::dzfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), double PN(scale),
	    double* PN(x), int32_t PN(ldx), DComplex* PN(y), int32_t PN(ldy),
	    double* PN(table), double* PN(work), int32_t PN(isys)) {
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfftm_((int*) &isign, (int*) &n, (int*) &lot, (double*) &scale,
	  (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	  table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::csfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), float PN(scale),
	    Complex* PN(x), int32_t PN(ldx), float* PN(y), int32_t PN(ldy),
	    float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  csfftm_((int*) &isign, (int*) &n, (int*) &lot, (float*) &scale,
	  (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	  table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::zdfftm(int32_t PN(isign), int32_t PN(n), int32_t PN(lot), double PN(scale),
	    DComplex* PN(x), int32_t PN(ldx), double* PN(y), int32_t PN(ldy),
	    double* PN(table), double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfftm_((int*) &isign, (int*) &n, (int*) &lot, (double*) &scale,
	  (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	  table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::ccfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), float PN(scale),
	     Complex* PN(x), int32_t PN(ldx), Complex* PN(y), int32_t PN(ldy),
	     float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  ccfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) &scale,
	   (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	   table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::zzfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), double PN(scale),
	     DComplex* PN(x), int32_t PN(ldx), DComplex* PN(y), int32_t PN(ldy),
	     double* PN(table), double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  zzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) &scale,
	   (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	   table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::scfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), float PN(scale),
	     float* PN(x), int32_t PN(ldx), Complex* PN(y), int32_t PN(ldy),
	     float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  scfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) &scale,
	   (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	   table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::dzfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), double PN(scale),
	     double* PN(x), int32_t PN(ldx), DComplex* PN(y), int32_t PN(ldy),
	     double* PN(table), double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) &scale,
	   (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	   table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::csfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), float PN(scale),
	     Complex* PN(x), int32_t PN(ldx), float* PN(y), int32_t PN(ldy),
	     float* PN(table), float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  csfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) &scale,
	   (float*) x, (int*) &ldx, (float*) y, (int*) &ldy, (float*)
	   table, (float*) work, (int*) &isys); 
#endif
}

void SCSL::zdfft2d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), double PN(scale),
	     DComplex* PN(x), int32_t PN(ldx), double* PN(y), int32_t PN(ldy),
	     double* PN(table), double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) &scale,
	   (double*) x, (int*) &ldx, (double*) y, (int*) &ldy, (double*)
	   table, (double*) work, (int*) &isys); 
#endif
}

void SCSL::ccfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     float PN(scale), Complex* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     Complex* PN(y), int32_t PN(ldy), int32_t PN(ldy2), float* PN(table),
	     float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  ccfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   &scale, (float*) x, (int*) &ldx, (int*) &ldx2, (float*) y,
	   (int*) &ldy, (int*) &ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
#endif
}

void SCSL::zzfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     double PN(scale), DComplex* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     DComplex* PN(y), int32_t PN(ldy), int32_t PN(ldy2), double* PN(table),
	     double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  zzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) &scale, (double*) x, (int*) &ldx, (int*) &ldx2,
	   (double*) y, (int*) &ldy, (int*) &ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
#endif
}

void SCSL::scfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     float PN(scale), float* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     Complex* PN(y), int32_t PN(ldy), int32_t PN(ldy2), float* PN(table),
	     float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  scfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   &scale, (float*) x, (int*) &ldx, (int*) &ldx2, (float*) y,
	   (int*) &ldy, (int*) &ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
#endif
}

void SCSL::dzfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     double PN(scale), double* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     DComplex* PN(y), int32_t PN(ldy), int32_t PN(ldy2), double* PN(table),
	     double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) &scale, (double*) x, (int*) &ldx, (int*) &ldx2,
	   (double*) y, (int*) &ldy, (int*) &ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
#endif
}

void SCSL::csfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     float PN(scale), Complex* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     float* PN(y), int32_t PN(ldy), int32_t PN(ldy2), float* PN(table),
	     float* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(Complex) == 2*sizeof(float), AipsError);
  csfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   &scale, (float*) x, (int*) &ldx, (int*) &ldx2, (float*) y,
	   (int*) &ldy, (int*) &ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
#endif
}

void SCSL::zdfft3d(int32_t PN(isign), int32_t PN(n1), int32_t PN(n2), int32_t PN(n3),
	     double PN(scale), DComplex* PN(x), int32_t PN(ldx), int32_t PN(ldx2),
	     double* PN(y), int32_t PN(ldy), int32_t PN(ldy2), double* PN(table),
	     double* PN(work), int32_t PN(isys)) { 
#if defined(HAVE_SCSL)
  DebugAssert(sizeof(DComplex) == 2*sizeof(double), AipsError);
  dzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) &scale, (double*) x, (int*) &ldx, (int*) &ldx2,
	   (double*) y, (int*) &ldy, (int*) &ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
#endif
}

} //# NAMESPACE CASACORE - END

