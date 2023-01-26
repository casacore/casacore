//# ConvolveGridder.cc: Convolutional Gridder
//# Copyright (C) 1996,1997,1999,2002,2003
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

#ifndef SCIMATH_CONVOLVEGRIDDER_TCC
#define SCIMATH_CONVOLVEGRIDDER_TCC

#include <casacore/scimath/Mathematics/ConvolveGridder.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define NEED_UNDERSCORES
#if defined(NEED_UNDERSCORES)
#define grdsf grdsf_
#define cgrd1d cgrd1d_
#define cgrd2d cgrd2d_
#define cgrd3d cgrd3d_
#define cdgrd1d cdgrd1d_
#define cdgrd2d cdgrd2d_
#define cdgrd3d cdgrd3d_

#define dgrd1d dgrd1d_
#define dgrd2d dgrd2d_
#define dgrd3d dgrd3d_
#define ddgrd1d ddgrd1d_
#define ddgrd2d ddgrd2d_
#define ddgrd3d ddgrd3d_

#define fgrd1d fgrd1d_
#define fgrd2d fgrd2d_
#define fgrd3d fgrd3d_
#define fdgrd1d fdgrd1d_
#define fdgrd2d fdgrd2d_
#define fdgrd3d fdgrd3d_

#endif

extern "C" { 
   void grdsf(double*, double*);
   void cgrd1d(int32_t*, int32_t*, Complex*, const Complex*, int32_t*, int32_t*, double*,
		 double*);
   void cgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, Complex*, const Complex*, int32_t*,
		 int32_t*, double*, double*, double*);
   void cgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, Complex*,
		 const Complex*, int32_t*, int32_t*, double*, double*, double*,
		 double*);
   void cdgrd1d(int32_t*, int32_t*, const Complex*, Complex*, int32_t*, int32_t*, double*,
		 double*);
   void cdgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, const Complex*, Complex*, int32_t*,
		 int32_t*, double*, double*, double*);
   void cdgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, const Complex*,
		 Complex*, int32_t*, int32_t*, double*, double*, double*,
		 double*);

   void fgrd1d(int32_t*, int32_t*, float*, const float*, int32_t*, int32_t*, double*,
		 double*);
   void fgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, float*, const float*, int32_t*,
		 int32_t*, double*, double*, double*);
   void fgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, float*,
		 const float*, int32_t*, int32_t*, double*, double*, double*,
		 double*);
   void fdgrd1d(int32_t*, int32_t*, const float*, float*, int32_t*, int32_t*, double*,
		 double*);
   void fdgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, const float*, float*, int32_t*,
		 int32_t*, double*, double*, double*);
   void fdgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, const float*,
		 float*, int32_t*, int32_t*, double*, double*, double*,
		 double*);

   void dgrd1d(int32_t*, int32_t*, double*, const double*, int32_t*, int32_t*, double*,
		 double*);
   void dgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, double*, const double*, int32_t*,
		 int32_t*, double*, double*, double*);
   void dgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, double*,
		 const double*, int32_t*, int32_t*, double*, double*, double*,
		 double*);
   void ddgrd1d(int32_t*, int32_t*, const double*, double*, int32_t*, int32_t*, double*,
		 double*);
   void ddgrd2d(int32_t*, int32_t*, int32_t*, int32_t*, const double*, double*, int32_t*,
		 int32_t*, double*, double*, double*);
   void ddgrd3d(int32_t*, int32_t*, int32_t *, int32_t*, int32_t*, int32_t *, const double*,
		 double*, int32_t*, int32_t*, double*, double*, double*,
		 double*);
}

// double versions

inline void grd1d(int32_t* ni, int32_t* li,
		  double* grid, const double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc) 
{
  dgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  double* grid, const double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  dgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  double* grid, const double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  dgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(int32_t* ni, int32_t* li,
		  const double* grid, double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc)
{
  ddgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  const double* grid, double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  ddgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  const double* grid, double* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  ddgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	  posi, posj, posk, convFunc);
}

// Complex versions

inline void grd1d(int32_t* ni, int32_t* li,
		  Complex* grid, const Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc) 
{
  cgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  Complex* grid, const Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  cgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  Complex* grid, const Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  cgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(int32_t* ni, int32_t* li,
		  const Complex* grid, Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc)
{
  cdgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  const Complex* grid, Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  cdgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  const Complex* grid, Complex* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  cdgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	  posi, posj, posk, convFunc);
}

// float versions

inline void grd1d(int32_t* ni, int32_t* li,
		  float* grid, const float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc) 
{
  fgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  float* grid, const float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  fgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  float* grid, const float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  fgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(int32_t* ni, int32_t* li,
		  const float* grid, float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi,
		  double* convFunc)
{
  fdgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(int32_t* ni, int32_t* nj, int32_t* li, int32_t* lj,
		  const float* grid, float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj,
		  double* convFunc)
{
  fdgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(int32_t* ni, int32_t* nj, int32_t *nk, int32_t* li, int32_t* lj, int32_t* lk,
		  const float* grid, float* value,
		  int32_t* sampling, int32_t* support,
		  double* posi, double* posj, double* posk,
		  double* convFunc)
{
  fdgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	  posi, posj, posk, convFunc);
}

template <class Domain, class Range>
ConvolveGridder<Domain, Range>::ConvolveGridder(const IPosition& shape,
				    const Vector<Domain>& scale,
				    const Vector<Domain>& offset,
				    const String& convType) 
: Gridder<Domain, Range>(shape, scale, offset)
{
  setConvolutionFunction(convType);
  fillCorrectionVectors();
  loc.resize(ndim);
  loc=0;
  supportVec.resize(ndim);
  supportVec=support;
}

template <class Domain, class Range>
bool ConvolveGridder<Domain, Range>::grid(Array<Range> &gridded,
				    const Vector<Domain>& p,
				    const Range& value)
{
  loc=this->location(loc,p);
  loc-=offsetVec;
  if(onGrid(loc,supportVec)) {
    bool del;
    posVec=this->position(posVec, p);
    const IPosition& fs = gridded.shape();
    std::vector<int32_t> s(fs.begin(), fs.end());
    switch(loc.nelements()) {
    case 1:
      grd1d(&s[0], &loc(0), gridded.getStorage(del), &value, &support,
	       &sampling, &posVec(0), convFunc.getStorage(del));
      break;
    case 2:
      grd2d(&s[0], &s[1], &loc(0), &loc(1), gridded.getStorage(del),
	       &value, &support, &sampling, &posVec(0), &posVec(1),
	       convFunc.getStorage(del));
      break;
    case 3:
      grd3d(&s[0], &s[1], &s[2], &loc(0), &loc(1), &loc(2),
	       gridded.getStorage(del), &value, &support,
	       &sampling, &posVec(0), &posVec(1), &posVec(2),
	       convFunc.getStorage(del));
      break;
    default:
      return false;
      break;
    }
    return true;
  }
  else {
    cout<<"Off grid"<<endl;
    return false;
  }
}

template <class Domain, class Range>
bool ConvolveGridder<Domain, Range>::degrid(const Array<Range> &gridded,
				    const Vector<Domain>& p,
				    Range& value)
{
  loc=this->location(loc,p); 
 if(onGrid(loc,supportVec)) {
    bool del;
    posVec=this->position(posVec, p);
    const IPosition& fs = gridded.shape();
    std::vector<int32_t> s(fs.begin(), fs.end());
    switch(loc.nelements()) {
    case 1:
      dgrd1d(&s[0], &loc(0), gridded.getStorage(del), &value, &support,
	       &sampling, &posVec(0), convFunc.getStorage(del));
      break;
    case 2:
      dgrd2d(&s[0], &s[1], &loc(0), &loc(1), gridded.getStorage(del),
	       &value, &support, &sampling, &posVec(0), &posVec(1),
	       convFunc.getStorage(del));
      break;
    case 3:
      dgrd3d(&s[0], &s[1], &s[2], &loc(0), &loc(1), &loc(2),
	       gridded.getStorage(del), &value, &support,
	       &sampling, &posVec(0), &posVec(1), &posVec(2),
	       convFunc.getStorage(del));
      break;
    default:
      return false;
      break;
    }
    return true;
  }
  else {
    return false;
  }
}

template <class Domain, class Range>
Range ConvolveGridder<Domain, Range>::correctionFactor1D(int32_t loc, int32_t len)
{
  int32_t offset=loc-len/2;
  if(cType=="BOX") {
    if(offset!=0.0) {
      double arg=C::pi*double(offset)/double(len);
      return sin(arg)/arg;
    }
    else {
      return 1.0;
    }
  }
  else {
    double nu=abs(double(offset)/double(len/2));
    double val;
    grdsf(&nu, &val);
    return val;
  }
  return 1.0;
}

template <class Domain, class Range>
void ConvolveGridder<Domain, Range>::setConvolutionFunction(const String& type) {

  cType=type;
  if(type=="BOX") {
    support=0;
    sampling=100;
    convFunc.resize(sampling*(support+1));
    convFunc=0.0;
    for (int32_t i=0;i<sampling*(support+1);i++) {
      convFunc(i)=1.0;
    }
  }
  else {
    // SF
    support=3;
    sampling=100;
    convFunc.resize(sampling*(support+1));
    convFunc=0.0;
    for (int32_t i=0;i<sampling*support;i++) {
      double nu=double(i)/double(support*sampling);
      double val;
      grdsf(&nu, &val);
      convFunc(i)=(1.0-nu*nu)*val;
    }
  }
}

template <class Domain, class Range>
Vector<double>& ConvolveGridder<Domain, Range>::cFunction() {
  return convFunc;
}

template <class Domain, class Range>
Vector<int32_t>& ConvolveGridder<Domain, Range>::cSupport() {
  return supportVec;
}

template <class Domain, class Range>
int32_t& ConvolveGridder<Domain, Range>::cSampling() {
  return sampling;
}

} //# NAMESPACE CASACORE - END


#endif
