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
//#

#include <aips/Mathematics.h>

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

void ccfft(Int isign, Int n, Float* scale, Float* x, Float* y,
	   Float* table, Float* work, Int isys) { 
  ccfft_((int*) &isign, (int*) &n, (float*) scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
  
}

void ccfft(Int isign, Int n, Double* scale, Double* x, Double* y,
	   Double* table, Double* work, Int isys) { 
  zzfft_((int*) &isign, (int*) &n, (double*) scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*)
	 &isys); 
}

void scfft(Int isign, Int n, Float* scale, Float* x, Float* y,
	   Float* table, Float* work, Int isys) { 
  scfft_((int*) &isign, (int*) &n, (float*) scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
}

void scfft(Int isign, Int n, Double* scale, Double* x, Double* y,
	   Double* table, Double* work, Int isys) { 
  dzfft_((int*) &isign, (int*) &n, (double*) scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*)
	 &isys); 
}

void csfft(Int isign, Int n, Float* scale, Float* x, Float* y,
	   Float* table, Float* work, Int isys) { 
  csfft_((int*) &isign, (int*) &n, (float*) scale, (float*) x,
	 (float*) y, (float*) table, (float*) work, (int*) &isys); 
}

void zdfft(Int isign, Int n, Float* scale, Float* x, Float* y,
	   Float* table, Float* work, Int isys) { 
  zdfft_((int*) &isign, (int*) &n, (double*) scale, (double*) x,
	 (double*) y, (double*) table, (double*) work, (int*)
	 &isys); 
}

void ccfftm(Int isign, Int n, Int lot, Float* scale, Float* x, Int
	    ldx, Float* y, Int ldy, Float* table, Float* work, Int
	    isys) { 
  ccfftm_((int*) &isign, (int*) &n, (int*) lot, (float*) scale,
	  (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	  table, (float*) work, (int*) &isys); 
}

void zzfftm(Int isign, Int n, Int lot, Double* scale, Double* x, Int
	    ldx, Double* y, Int ldy, Double* table, Double* work, Int
	    isys) {
  zzfftm_((int*) &isign, (int*) &n, (int*) lot, (double*) scale,
	  (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	  table, (double*) work, (int*) &isys);
}

void scfftm(Int isign, Int n, Int lot, Float* scale, Float* x, Int
	    ldx, Float* y, Int ldy, Float* table, Float* work, Int
	    isys) {
  scfftm_((int*) &isign, (int*) &n, (int*) lot, (float*) scale,
	  (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	  table, (float*) work, (int*) &isys);
}

void dzfftm(Int isign, Int n, Int lot, Double* scale, Double* x, Int
	    ldx, Double* y, Int ldy, Double* table, Double* work, Int
	    isys) {
  dzfftm_((int*) &isign, (int*) &n, (int*) lot, (double*) scale,
	  (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	  table, (double*) work, (int*) &isys); 
}

void csfftm(Int isign, Int n, Int lot, Float* scale, Float* x, Int
	    ldx, Float* y, Int ldy, Float* table, Float* work, Int
	    isys) { 
  csfftm_((int*) &isign, (int*) &n, (int*) lot, (float*) scale,
	  (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	  table, (float*) work, (int*) &isys); 
}

void zdfftm(Int isign, Int n, Int lot, Double* scale, Double* x, Int
	    ldx, Double* y, Int ldy, Double* table, Double* work, Int
	    isys) { 
  dzfftm_((int*) &isign, (int*) &n, (int*) lot, (double*) scale,
	  (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	  table, (double*) work, (int*) &isys); 
}

void ccfft2d(Int isign, Int n1, Int n2, Float* scale, Float* x, Int
	     ldx, Float* y, Int ldy, Float* table, Float* work, Int
	     isys) { 
  ccfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) scale,
	   (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	   table, (float*) work, (int*) &isys); 
}

void zzfft2d(Int isign, Int n1, Int n2, Float* scale, Float* x, Int
	     ldx, Float* y, Int ldy, Float* table, Float* work, Int
	     isys) { 
  zzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) scale,
	   (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	   table, (double*) work, (int*) &isys); 
}

void scfft2d(Int isign, Int n1, Int n2, Float* scale, Float* x, Int
	     ldx, Float* y, Int ldy, Float* table, Float* work, Int
	     isys) { 
  scfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) scale,
	   (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	   table, (float*) work, (int*) &isys); 
}

void dzfft2d(Int isign, Int n1, Int n2, Double* scale, Double* x, Int
	     ldx, Double* y, Int ldy, Double* table, Double* work, Int
	     isys) { 
  dzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) scale,
	   (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	   table, (double*) work, (int*) &isys); 
}

void csfft2d(Int isign, Int n1, Int n2, Float* scale, Float* x, Int
	     ldx, Float* y, Int ldy, Float* table, Float* work, Int
	     isys) { 
  csfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (float*) scale,
	   (float*) x, (int*) ldx, (float*) y, (int*) ldy, (float*)
	   table, (float*) work, (int*) &isys); 
}

void zdfft2d(Int isign, Int n1, Int n2, Double* scale, Double* x, Int
	     ldx, Double* y, Int ldy, Double* table, Double* work, Int
	     isys) { 
  dzfft2d_((int*) &isign, (int*) &n1, (int*) &n2, (double*) scale,
	   (double*) x, (int*) ldx, (double*) y, (int*) ldy, (double*)
	   table, (double*) work, (int*) &isys); 
}

void ccfft3d(Int isign, Int n1, Int n2, Int n3, Float* scale, Float*
	     x, Int ldx, Int ldx2, Float* y, Int ldy, Int ldy2, Float*
	     table, Float* work, Int isys) { 
  ccfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   scale, (float*) x, (int*) ldx, (int*) ldx2, (float*) y,
	   (int*) ldy, (int*) ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
}

void zzfft3d(Int isign, Int n1, Int n2, Int n3, Float* scale, Float*
	     x, Int ldx, Int ldx2, Float* y, Int ldy, Int ldy2, Float*
	     table, Float* work, Int isys) { 
  zzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) scale, (double*) x, (int*) ldx, (int*) ldx2,
	   (double*) y, (int*) ldy, (int*) ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
}

void scfft3d(Int isign, Int n1, Int n2, Int n3, Float* scale, Float*
	     x, Int ldx, Int ldx2, Float* y, Int ldy, Int ldy2, Float*
	     table, Float* work, Int isys) { 
  scfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   scale, (float*) x, (int*) ldx, (int*) ldx2, (float*) y,
	   (int*) ldy, (int*) ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
}

void dzfft3d(Int isign, Int n1, Int n2, Int n3, Double* scale, Double*
	     x, Int ldx, Int ldx2, Double* y, Int ldy, Int ldy2,
	     Double* table, Double* work, Int isys) { 
  dzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) scale, (double*) x, (int*) ldx, (int*) ldx2,
	   (double*) y, (int*) ldy, (int*) ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
}

void csfft3d(Int isign, Int n1, Int n2, Int n3, Float* scale, Float*
	     x, Int ldx, Int ldx2, Float* y, Int ldy, Int ldy2, Float*
	     table, Float* work, Int isys) { 
  csfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3, (float*)
	   scale, (float*) x, (int*) ldx, (int*) ldx2, (float*) y,
	   (int*) ldy, (int*) ldy2, (float*) table, (float*) work,
	   (int*) &isys); 
}

void zdfft3d(Int isign, Int n1, Int n2, Int n3, Double* scale, Double*
	     x, Int ldx, Int ldx2, Double* y, Int ldy, Int ldy2,
	     Double* table, Double* work, Int isys) { 
  dzfft3d_((int*) &isign, (int*) &n1, (int*) &n2, (int*) &n3,
	   (double*) scale, (double*) x, (int*) ldx, (int*) ldx2,
	   (double*) y, (int*) ldy, (int*) ldy2, (double*) table,
	   (double*) work, (int*) &isys); 
}
