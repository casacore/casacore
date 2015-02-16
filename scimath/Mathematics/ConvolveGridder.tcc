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
//#
//#
//# $Id$

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
   void grdsf(Double*, Double*);
   void cgrd1d(Int*, Int*, Complex*, const Complex*, Int*, Int*, Double*,
		 Double*);
   void cgrd2d(Int*, Int*, Int*, Int*, Complex*, const Complex*, Int*,
		 Int*, Double*, Double*, Double*);
   void cgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, Complex*,
		 const Complex*, Int*, Int*, Double*, Double*, Double*,
		 Double*);
   void cdgrd1d(Int*, Int*, const Complex*, Complex*, Int*, Int*, Double*,
		 Double*);
   void cdgrd2d(Int*, Int*, Int*, Int*, const Complex*, Complex*, Int*,
		 Int*, Double*, Double*, Double*);
   void cdgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, const Complex*,
		 Complex*, Int*, Int*, Double*, Double*, Double*,
		 Double*);

   void fgrd1d(Int*, Int*, Float*, const Float*, Int*, Int*, Double*,
		 Double*);
   void fgrd2d(Int*, Int*, Int*, Int*, Float*, const Float*, Int*,
		 Int*, Double*, Double*, Double*);
   void fgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, Float*,
		 const Float*, Int*, Int*, Double*, Double*, Double*,
		 Double*);
   void fdgrd1d(Int*, Int*, const Float*, Float*, Int*, Int*, Double*,
		 Double*);
   void fdgrd2d(Int*, Int*, Int*, Int*, const Float*, Float*, Int*,
		 Int*, Double*, Double*, Double*);
   void fdgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, const Float*,
		 Float*, Int*, Int*, Double*, Double*, Double*,
		 Double*);

   void dgrd1d(Int*, Int*, Double*, const Double*, Int*, Int*, Double*,
		 Double*);
   void dgrd2d(Int*, Int*, Int*, Int*, Double*, const Double*, Int*,
		 Int*, Double*, Double*, Double*);
   void dgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, Double*,
		 const Double*, Int*, Int*, Double*, Double*, Double*,
		 Double*);
   void ddgrd1d(Int*, Int*, const Double*, Double*, Int*, Int*, Double*,
		 Double*);
   void ddgrd2d(Int*, Int*, Int*, Int*, const Double*, Double*, Int*,
		 Int*, Double*, Double*, Double*);
   void ddgrd3d(Int*, Int*, Int *, Int*, Int*, Int *, const Double*,
		 Double*, Int*, Int*, Double*, Double*, Double*,
		 Double*);
}

// Double versions

inline void grd1d(Int* ni, Int* li,
		  Double* grid, const Double* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc) 
{
  dgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  Double* grid, const Double* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  dgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  Double* grid, const Double* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
{
  dgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(Int* ni, Int* li,
		  const Double* grid, Double* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc)
{
  ddgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  const Double* grid, Double* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  ddgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  const Double* grid, Double* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
{
  ddgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	  posi, posj, posk, convFunc);
}

// Complex versions

inline void grd1d(Int* ni, Int* li,
		  Complex* grid, const Complex* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc) 
{
  cgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  Complex* grid, const Complex* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  cgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  Complex* grid, const Complex* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
{
  cgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(Int* ni, Int* li,
		  const Complex* grid, Complex* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc)
{
  cdgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  const Complex* grid, Complex* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  cdgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  const Complex* grid, Complex* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
{
  cdgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	  posi, posj, posk, convFunc);
}

// Float versions

inline void grd1d(Int* ni, Int* li,
		  Float* grid, const Float* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc) 
{
  fgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}

inline void grd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  Float* grid, const Float* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  fgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void grd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  Float* grid, const Float* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
{
  fgrd3d(ni, nj, nk, li, lj, lk, grid, value, sampling, support,
	 posi, posj, posk, convFunc);
}



inline void dgrd1d(Int* ni, Int* li,
		  const Float* grid, Float* value,
		  Int* sampling, Int* support,
		  Double* posi,
		  Double* convFunc)
{
  fdgrd1d(ni, li, grid, value, sampling, support, posi, convFunc);
}
inline void dgrd2d(Int* ni, Int* nj, Int* li, Int* lj,
		  const Float* grid, Float* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj,
		  Double* convFunc)
{
  fdgrd2d(ni, nj, li, lj, grid, value, sampling, support, posi, posj, convFunc);
}

inline void dgrd3d(Int* ni, Int* nj, Int *nk, Int* li, Int* lj, Int* lk,
		  const Float* grid, Float* value,
		  Int* sampling, Int* support,
		  Double* posi, Double* posj, Double* posk,
		  Double* convFunc)
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
Bool ConvolveGridder<Domain, Range>::grid(Array<Range> &gridded,
				    const Vector<Domain>& p,
				    const Range& value)
{
  loc=this->location(loc,p);
  loc-=offsetVec;
  if(onGrid(loc,supportVec)) {
    Bool del;
    posVec=this->position(posVec, p);
    const IPosition& fs = gridded.shape();
    vector<Int> s(fs.begin(), fs.end());
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
      return False;
      break;
    }
    return True;
  }
  else {
    cout<<"Off grid"<<endl;
    return False;
  }
}

template <class Domain, class Range>
Bool ConvolveGridder<Domain, Range>::degrid(const Array<Range> &gridded,
				    const Vector<Domain>& p,
				    Range& value)
{
  loc=this->location(loc,p); 
 if(onGrid(loc,supportVec)) {
    Bool del;
    posVec=this->position(posVec, p);
    const IPosition& fs = gridded.shape();
    vector<Int> s(fs.begin(), fs.end());
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
      return False;
      break;
    }
    return True;
  }
  else {
    return False;
  }
}

template <class Domain, class Range>
Range ConvolveGridder<Domain, Range>::correctionFactor1D(Int loc, Int len)
{
  Int offset=loc-len/2;
  if(cType=="BOX") {
    if(offset!=0.0) {
      Double arg=C::pi*Double(offset)/Double(len);
      return sin(arg)/arg;
    }
    else {
      return 1.0;
    }
  }
  else {
    Double nu=abs(Double(offset)/Double(len/2));
    Double val;
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
    for (Int i=0;i<sampling*(support+1);i++) {
      convFunc(i)=1.0;
    }
  }
  else {
    // SF
    support=3;
    sampling=100;
    convFunc.resize(sampling*(support+1));
    convFunc=0.0;
    for (Int i=0;i<sampling*support;i++) {
      Double nu=Double(i)/Double(support*sampling);
      Double val;
      grdsf(&nu, &val);
      convFunc(i)=(1.0-nu*nu)*val;
    }
  }
}

template <class Domain, class Range>
Vector<Double>& ConvolveGridder<Domain, Range>::cFunction() {
  return convFunc;
}

template <class Domain, class Range>
Vector<Int>& ConvolveGridder<Domain, Range>::cSupport() {
  return supportVec;
}

template <class Domain, class Range>
Int& ConvolveGridder<Domain, Range>::cSampling() {
  return sampling;
}

} //# NAMESPACE CASACORE - END


#endif
