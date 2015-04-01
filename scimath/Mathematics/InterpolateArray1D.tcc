//# Interpolate1DArray.cc:  implements Interpolation in one dimension
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef SCIMATH_INTERPOLATEARRAY1D_TCC
#define SCIMATH_INTERPOLATEARRAY1D_TCC

#include <casacore/scimath/Mathematics/InterpolateArray1D.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <limits>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolate(Array<Range>& yout, 
						   const Vector<Domain>& xout,
						   const Vector<Domain>& xin, 
						   const Array<Range>& yin,
						   Int method)
{
  const uInt ndim = yin.ndim();
  Int nxin=xin.nelements(), nxout=xout.nelements();
  IPosition yinShape=yin.shape();
  DebugAssert(nxin==yinShape(ndim-1),AipsError);

  Bool deleteYin, deleteYout;
  const Range* pyin=yin.getStorage(deleteYin);
  Int yStep=1; 
  Int i;
  for (i=0; i<Int(ndim)-1; i++) yStep*=yinShape(i);
  IPosition youtShape=yinShape;
  youtShape(ndim-1)=nxout;
  yout.resize(youtShape);
  Range* pyout=yout.getStorage(deleteYout);

  PtrBlock<const Range*> yinPtrs(nxin);
  PtrBlock<Range*> youtPtrs(nxout);
  for (i=0; i<nxin; i++) yinPtrs[i]=pyin+i*yStep;
  for (i=0; i<nxout; i++) youtPtrs[i]=pyout+i*yStep;
  
  interpolatePtr(youtPtrs, yStep, xout, xin, yinPtrs, method);

  yin.freeStorage(pyin,deleteYin);
  yout.putStorage(pyout,deleteYout);
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolate(Array<Range>& yout, 
						   const Block<Domain>& xout,
						   const Block<Domain>& xin, 
						   const Array<Range>& yin,
						   Int method)
{
  Vector<Domain> vxout(xout);
  Vector<Domain> vxin(xin);
  interpolate(yout,vxout,vxin,yin,method);
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolate(Array<Range>& yout, 
						   Array<Bool>& youtFlags,
						   const Vector<Domain>& xout,
						   const Vector<Domain>& xin, 
						   const Array<Range>& yin,
						   const Array<Bool>& yinFlags,
						   Int method,
                                                   Bool goodIsTrue,
						   Bool extrapolate)
{
  const uInt ndim = yin.ndim();
  Int nxin=xin.nelements(), nxout=xout.nelements();
  IPosition yinShape=yin.shape();
  DebugAssert(nxin==yinShape(ndim-1),AipsError);
  DebugAssert((yinFlags.shape() == yinShape), AipsError);

  Bool deleteYin, deleteYout, deleteYinFlags, deleteYoutFlags;
  const Range* pyin=yin.getStorage(deleteYin);
  const Bool* pyinFlags=yinFlags.getStorage(deleteYinFlags);
  Int yStep=1; 
  Int i;
  for (i=0; i<Int(ndim)-1; i++) yStep*=yinShape(i);
  IPosition youtShape=yinShape;
  youtShape(ndim-1)=nxout;
  yout.resize(youtShape);
  youtFlags.resize(youtShape);
  youtFlags.set(False);
  Range* pyout=yout.getStorage(deleteYout);
  Bool* pyoutFlags=youtFlags.getStorage(deleteYoutFlags);

  PtrBlock<const Range*> yinPtrs(nxin);
  PtrBlock<const Bool*> yinFlagPtrs(nxin);
  PtrBlock<Range*> youtPtrs(nxout);
  PtrBlock<Bool*> youtFlagPtrs(nxout);
  for (i=0; i<nxin; i++) {
    yinPtrs[i]=pyin+i*yStep;
    yinFlagPtrs[i]=pyinFlags+i*yStep;
  }
  for (i=0; i<nxout; i++) {
    youtPtrs[i]=pyout+i*yStep;
    youtFlagPtrs[i]=pyoutFlags+i*yStep;
  }
  interpolatePtr(youtPtrs, youtFlagPtrs, yStep, xout, xin, yinPtrs,
		 yinFlagPtrs, method, goodIsTrue, extrapolate);

  yin.freeStorage(pyin,deleteYin);
  yinFlags.freeStorage(pyinFlags,deleteYinFlags);
  yout.putStorage(pyout,deleteYout);
  youtFlags.putStorage(pyoutFlags,deleteYoutFlags);
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolate(Array<Range>& yout, 
						   Array<Bool>& youtFlags,
						   const Block<Domain>& xout,
						   const Block<Domain>& xin, 
						   const Array<Range>& yin,
						   const Array<Bool>& yinFlags,
						   Int method,
                                                   Bool goodIsTrue,
						   Bool extrapolate)
{
  Vector<Domain> vxout(xout);
  Vector<Domain> vxin(xin);
  interpolate(yout,youtFlags,vxout,vxin,yin,yinFlags,
	      method,goodIsTrue,extrapolate);
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolatey(Cube<Range>& yout,
                                                   const Vector<Domain>& xout,
                                                   const Vector<Domain>& xin,
                                                   const Cube<Range>& yin,
                                                   Int method)
{
  Int nxout=xout.nelements();
  IPosition yinShape=yin.shape();
  //check the number of elements in y
  DebugAssert(xin.nelements()==yinShape(2),AipsError);

  Bool deleteYin, deleteYout;
  const Range* pyin=yin.getStorage(deleteYin);
  Int na=yinShape(0);
  Int nb=yinShape(1);
  Int nc=yinShape(2);
  IPosition youtShape=yinShape;
  youtShape(1)=nxout;  // pick y of cube
  //youtShape(2)=nxout;  // pick z of cube
  yout.resize(youtShape);
  Range* pyout=yout.getStorage(deleteYout);

  PtrBlock<const Range*> yinPtrs(na*nb*nc);
  PtrBlock<Range*> youtPtrs(na*nxout*nc);
  Int i;
  for (i=0; i<(na*nb*nc); i++) yinPtrs[i]=pyin+i;
  for (i=0; i<(na*nxout*nc); i++) {
     youtPtrs[i]=pyout+i;
  }
  interpolateyPtr(youtPtrs, na, nb, nc, xout, xin, yinPtrs, method);

  yin.freeStorage(pyin,deleteYin);
  yout.putStorage(pyout,deleteYout);

}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolatey(Cube<Range>& yout,
                                                   Cube<Bool>& youtFlags,
                                                   const Vector<Domain>& xout,
                                                   const Vector<Domain>& xin,
                                                   const Cube<Range>& yin,
						   const Cube<Bool>& yinFlags,
                                                   Int method,
                                                   Bool goodIsTrue,
						   Bool extrapolate)
{
  Int nxout=xout.nelements();
  IPosition yinShape=yin.shape();
  DebugAssert(xin.nelements()==yinShape(2),AipsError);
  DebugAssert((yinFlags.shape() == yinShape), AipsError);

  Bool deleteYin, deleteYout, deleteYinFlags, deleteYoutFlags;
  const Range* pyin=yin.getStorage(deleteYin);
  const Bool* pyinFlags=yinFlags.getStorage(deleteYinFlags);
  Int na=yinShape(0);
  Int nb=yinShape(1);
  Int nc=yinShape(2);
  IPosition youtShape=yinShape;
  youtShape(1)=nxout;  // pick y of cube
  yout.resize(youtShape);
  youtFlags.resize(youtShape);
  youtFlags.set(False);
  Range* pyout=yout.getStorage(deleteYout);
  Bool* pyoutFlags=youtFlags.getStorage(deleteYoutFlags);

  PtrBlock<const Range*> yinPtrs(na*nb*nc);
  PtrBlock<const Bool*> yinFlagPtrs(na*nb*nc);
  PtrBlock<Range*> youtPtrs(na*nxout*nc);
  PtrBlock<Bool*> youtFlagPtrs(na*nxout*nc);
  Int i;
  for (i=0; i<(na*nb*nc); i++) {
    yinPtrs[i]=pyin+i;
    yinFlagPtrs[i]=pyinFlags+i;
  }
  for (i=0; i<(na*nxout*nc); i++) {
     youtPtrs[i]=pyout+i;
    youtFlagPtrs[i]=pyoutFlags+i;
  }
  interpolateyPtr(youtPtrs, youtFlagPtrs, na, nb, nc, xout, xin, yinPtrs,
                  yinFlagPtrs, method, goodIsTrue, extrapolate);
  yin.freeStorage(pyin,deleteYin);
  yinFlags.freeStorage(pyinFlags,deleteYinFlags);
  yout.putStorage(pyout,deleteYout);
  youtFlags.putStorage(pyoutFlags,deleteYoutFlags);
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolatePtr(PtrBlock<Range*>& yout, 
                                                      Int ny, 
  					              const Vector<Domain>& xout, 
						      const Vector<Domain>& xin,
						      const PtrBlock<const Range*>& yin, 
                                                      Int method)
{
  uInt nElements=xin.nelements();
  AlwaysAssert (nElements>0, AipsError);
  Domain x_req;
  switch (method) {
  case nearestNeighbour: // This does nearest neighbour interpolation
    {
      for (uInt i=0; i<xout.nelements(); i++) {
	x_req=xout[i];
	Bool found;
	uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	if (where == nElements) {
	  for (Int j=0; j<ny; j++) yout[i][j]=yin[nElements-1][j];
	}
	else if (where == 0) {
	  for (Int j=0; j<ny; j++) yout[i][j]=yin[0][j];
	}
	else {
	  Domain diff=(xin[where]-x_req);
	  //static_cast required to get .5 coerced for SGI compiler:
	  if (diff < static_cast<Domain>(.5)*(xin[where]-xin[where-1])) { 
            // closer to next
	    for (Int j=0; j<ny; j++) yout[i][j]=yin[where][j];
	  }
	  else {
            // closer to previous
	    for (Int j=0; j<ny; j++) yout[i][j]=yin[where-1][j];
	  }
	}
      }
      return;
    }
  case linear: // Linear interpolation is the default
    {
      for (uInt i=0; i<xout.nelements(); i++) {
	x_req=xout[i];
	Bool found;
	uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	if (where == nElements)
	  where--;
	else if (where == 0)
	  where++;
	Domain x2 = xin[where]; Int ind2 = where;
	where--;
	Domain x1 = xin[where]; Int ind1 = where; 
	if (nearAbs(x1, x2)) 
	  throw(AipsError("Interpolate1D::operator()"
			  " data has repeated x values"));
	Domain frac=(x_req-x1)/(x2-x1);
	for (Int j=0; j<ny; j++) 
	  yout[i][j] = yin[ind1][j] + frac * (yin[ind2][j] - yin[ind1][j]);
	//    return y1 + ((x_req-x1)/(x2-x1)) * (y2-y1);
      }
      return ;
    }
  case cubic:// fit a cubic polynomial to the four nearest points
    {
      polynomialInterpolation(yout, ny, xout, xin, yin, 3);
      return;
    }
  case spline: // natural cubic splines
    {
      Block<Range> y2(nElements);
      // The y2 values are initialised here.  I need to calculate the second
      // derivates of the interpolating curve at each x_value.  As described
      // in Numerical Recipies 2nd Ed. Sec. 3.3, this is done by requiring
      // that the first derivative is continuous at each data point. This
      // leads to a set of equations that has a tridiagonal form that can be
      // solved using an order(N) algorithm.
      //
      // The first part of this solution is to do the Gaussian elimination so
      // that all the coefficients on the diagonal are one, and zero below the
      // diagonal.  Because the system is tridiagonal the only non-zero
      // coefficients are in the diagonal immediately above the main
      // one. These values are stored in y2Values temporarily. The temporary
      // storage t, is used to hold the right hand side.
      Block<Domain> t(nElements);
      t[0] = 0; 
      for (Int j=0; j<ny; j++) {
	y2[0] = Range(0);
	y2[nElements-1] = y2[0];
	Domain c = xin[1] - xin[0];
	if (nearAbs(xin[1],  xin[0])) 
	  throw(AipsError("Interpolate1D::setMethod"
			  " data has repeated x values"));
	Domain a, b, delta;
	const Domain six = 6;
	const Float one = 1;
	Range r;
	Int i;
	for (i = 1; i < Int(nElements)-1; i++){
	  a = c;
	  b = Domain(2)*(xin[i+1] - xin[i-1]);
	  if (nearAbs(xin[i+1],  xin[i])) 
	    throw(AipsError("Interpolate1D::setMethod"
			    " data has repeated x values"));
	  c = (xin[i+1] - xin[i]);
	  r = (one/c) * (yin[i+1][j] - yin[i][j]) - 
	    (one/a) * (yin[i][j] - yin[i-1][j]);
	  delta = a * t[i-1];
	  if (nearAbs(b, delta)) 
	    throw(AipsError("Interpolate1D::setMethod"
			    " trouble constructing second derivatives"));
	  delta = b - delta;
	  t[i] = c/delta;
	  y2[i] = (one/delta)*(six*r - a*y2[i-1]);
	}
	// The second part of the solution is to do the back-substitution to
	// iteratively obtain the second derivatives.
	for (i = Int(nElements)-2; i > 1; i--){
	  y2[i] -= t[i]*y2[i+1];
	}

	for (i=0; i<Int(xout.nelements()); i++) {
	  x_req=xout[i];
	  Bool found;
	  uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	  if (where == nElements)
	    where--;
	  else if (where == 0)
	    where++;
	  
	  Domain dx, h, a, b, x1, x2;
	  Range y1v, y2v, y1d, y2d;
	  
	  x2 = xin[where]; 
	  y2v = yin[where][j];
	  y2d = y2[where];
	  where--;
	  x1 = xin[where]; 
	  y1v = yin[where][j];
	  y1d = y2[where];
	  if (nearAbs(x1, x2)) 
	    throw(AipsError("Interpolate1D::operator()"
			    " data has repeated x values"));
	  dx = x2-x1;
	  a = (x2-x_req)/dx; 
	  b = Domain(1)-a;
	  h = dx*dx/6.;
	  yout[i][j] = a*y1v + b*y2v + h*(a*a*a-a)*y1d + h*(b*b*b-b)*y2d;
	}
      }
      return;
    }
  }
}  

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolatePtr(PtrBlock<Range*>& yout, 
						      PtrBlock<Bool*>& youtFlags, 
						      Int ny, 
 						      const Vector<Domain>& xout, 
						      const Vector<Domain>& xin,
						      const PtrBlock<const Range*>& yin, 
						      const PtrBlock<const Bool*>& yinFlags, 
						      Int method,
						      Bool goodIsTrue,
						      Bool extrapolate)
{
  uInt nElements=xin.nelements();
  Domain x_req;
  Bool flag = !(goodIsTrue);
  switch (method) {
  case nearestNeighbour: // This does nearest neighbour interpolation
    {
      for (Int i=0; i<Int(xout.nelements()); i++) {
	x_req=xout[i];
	Bool found;
	uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	if (where == nElements) {
	  for (Int j=0; j<ny; j++) {
	    yout[i][j]=yin[nElements-1][j];
	    youtFlags[i][j]=(extrapolate ? yinFlags[nElements-1][j] : flag);
	  }
	}
	else if (where == 0) {
	  for (Int j=0; j<ny; j++) {
	    yout[i][j]=yin[0][j];
	    youtFlags[i][j]=((x_req==xin[0])||extrapolate ? yinFlags[0][j] : flag);
	  } 
	}
	else {
	  Domain diff=(xin[where] - x_req);
	  //static_cast required to get .5 coerced for SGI compiler:
	  if (diff < static_cast<Domain>(.5)*(xin[where]-xin[where-1])) { 
            // closer to next
	    for (Int j=0; j<ny; j++) {
	      yout[i][j]=yin[where][j];
	      youtFlags[i][j]=yinFlags[where][j];
	    }
	  }
	  else {
            // closer to previous
	    for (Int j=0; j<ny; j++) {
	      yout[i][j]=yin[where-1][j];
	      youtFlags[i][j]=yinFlags[where-1][j];
	    }
	  }
	}
      }
      return;
    }
  case linear: // Linear interpolation is the default
    {
      for (Int i=0; i<Int(xout.nelements()); i++) {
	x_req=xout[i];
	Bool found;
	Bool discard = False;
	uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	if (where == nElements) {
	  discard=!extrapolate;
	  where--;
	} 
	else if (where == 0) {
	  discard=(x_req!=xin[0])&&(!extrapolate);
	  where++;
	}
	Domain x2 = xin[where]; Int ind2 = where;
	where--;
	Domain x1 = xin[where]; Int ind1 = where; 
	if (nearAbs(x1, x2)) 
	  throw(AipsError("Interpolate1D::operator()"
			  " data has repeated x values"));
	Domain frac=(x_req-x1)/(x2-x1);
	Domain limit = std::numeric_limits<Domain>::epsilon();

//    y1 + ((x_req-x1)/(x2-x1)) * (y2-y1);
        if (frac>limit && frac<1.-limit) {
	  //cout << "two: frac "  << setprecision(12) << xfrac << endl;
          if (goodIsTrue) {
            for (Int j=0; j<ny; j++) {
              yout[i][j] = yin[ind1][j] + frac * (yin[ind2][j] - yin[ind1][j]);
              youtFlags[i][j] = (discard ? flag : 
                                 yinFlags[ind1][j] && yinFlags[ind2][j]);
            }
          } else {
            for (Int j=0; j<ny; j++) {
              yout[i][j] = yin[ind1][j] + frac * (yin[ind2][j] - yin[ind1][j]);
              youtFlags[i][j] = ( discard ? flag : 
                                  yinFlags[ind1][j] || yinFlags[ind2][j]);
            }
          }
        } else {
          // only one of the channels is involved
	  //cout << "one: frac "  << setprecision(12) << xfrac << endl;
	  if (frac<=limit) {
	    for (Int j=0; j<ny; j++) {
	      yout[i][j] = yin[ind1][j];
	      youtFlags[i][j] = (discard ? flag : yinFlags[ind1][j]);
	    }
	  } else { // frac >= 1.-limit
	    for (Int j=0; j<ny; j++) {
	      yout[i][j] = yin[ind2][j];
	      youtFlags[i][j] = (discard ? flag :  yinFlags[ind2][j]);
	    }
	  }
	}	  
          
      }
      return ;
    }
  case cubic:// fit a cubic polynomial to the four nearest points
    {
      // TODO: implement flags properly - ie don't use flagged points
      polynomialInterpolation(yout, ny, xout, xin, yin, 3);
      for (uInt i=0; i<xout.nelements(); i++) {
	Domain x_req=xout[i];
	Bool found;
	Bool discard = False;
	uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	if (where == nElements) {
	  where--;
	  discard = !extrapolate;
	}
	else if (where == 0) {
	  where++;
	  discard=(x_req!=xin[0])&&(!extrapolate);
	}
	Int ind2 = where;
	where--;
	Int ind1 = where; 
        if (goodIsTrue) {
	  for (Int j=0; j<ny; j++) {
	    youtFlags[i][j] = (discard ? 
			       flag : yinFlags[ind1][j] && yinFlags[ind2][j]);
	  }
        } else {
	  for (Int j=0; j<ny; j++) {
	    youtFlags[i][j] = (discard ? 
			       flag : yinFlags[ind1][j] || yinFlags[ind2][j]);
	  }
        }
      }
      return;
    }
  case spline: // natural cubic splines
    {
      // TODO: implement flags properly - ie don't use flagged points
      Block<Range> y2(nElements);
      // The y2 values are initialised here.  I need to calculate the second
      // derivates of the interpolating curve at each x_value.  As described
      // in Numerical Recipies 2nd Ed. Sec. 3.3, this is done by requiring
      // that the first derivative is continuous at each data point. This
      // leads to a set of equations that has a tridiagonal form that can be
      // solved using an order(N) algorithm.
      //
      // The first part of this solution is to do the Gaussian elimination so
      // that all the coefficients on the diagonal are one, and zero below the
      // diagonal.  Because the system is tridiagonal the only non-zero
      // coefficients are in the diagonal immediately above the main
      // one. These values are stored in y2Values temporarily. The temporary
      // storage t, is used to hold the right hand side.
      Block<Domain> t(nElements);
      t[0] = 0; 
      for (Int j=0; j<ny; j++) {
	y2[0] = Range(0);
	y2[nElements-1] = y2[0];
	Domain c = xin[1] - xin[0];
	if (nearAbs(xin[1],  xin[0])) 
	  throw(AipsError("Interpolate1D::setMethod"
			  " data has repeated x values"));
	Domain a, b, delta;
	const Domain six = 6;
	const Float one = 1;
	Range r;
	Int i;
	for (i = 1; i < Int(nElements)-1; i++){
	  a = c;
	  b = Domain(2)*(xin[i+1] - xin[i-1]);
	  if (nearAbs(xin[i+1],  xin[i])) 
	    throw(AipsError("Interpolate1D::setMethod"
			    " data has repeated x values"));
	  c = (xin[i+1] - xin[i]);
	  r = (one/c) * (yin[i+1][j] - yin[i][j]) - 
	    (one/a) * (yin[i][j] - yin[i-1][j]);
	  delta = a * t[i-1];
	  if (nearAbs(b, delta)) 
	    throw(AipsError("Interpolate1D::setMethod"
			    " trouble constructing second derivatives"));
	  delta = b - delta;
	  t[i] = c/delta;
	  y2[i] = (one/delta)*(six*r - a*y2[i-1]);
	}
	// The second part of the solution is to do the back-substitution to
	// iteratively obtain the second derivatives.
	for (i = nElements-2; i > 1; i--){
	  y2[i] -= t[i]*y2[i+1];
	}

	for (i=0; i<Int(xout.nelements()); i++) {
	  x_req=xout[i];
	  Bool found;
	  Bool discard = False;
	  uInt where = binarySearchBrackets(found, xin, x_req, nElements);
	  if (where == nElements) {
	    where--;
	    discard = !extrapolate;
	  }
	  else if (where == 0) {
	    where++;
	    discard=(x_req!=xin[0])&&(!extrapolate);
	  }
	  
	  Domain dx, h, a, b, x1, x2;
	  Range y1v, y2v, y1d, y2d;
	  
	  x2 = xin[where]; 
	  y2v = yin[where][j];
	  y2d = y2[where];
	  Bool f2 = yinFlags[where][j];
	  where--;
	  x1 = xin[where]; 
	  y1v = yin[where][j];
	  y1d = y2[where];
	  Bool f1 = yinFlags[where][j];
	  if (nearAbs(x1, x2)) 
	    throw(AipsError("Interpolate1D::operator()"
			    " data has repeated x values"));
	  dx = x2-x1;
	  a = (x2-x_req)/dx; 
	  b = Domain(1)-a;
	  h = dx*dx/6.;
	  yout[i][j] = a*y1v + b*y2v + h*(a*a*a-a)*y1d + h*(b*b*b-b)*y2d;
	  if (goodIsTrue) youtFlags[i][j] = (discard ? flag : f1 && f2);
	  else youtFlags[i][j] = (discard ? flag : f1 || f2);
	}
      }
      return;
    }
  }
}  

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolateyPtr(PtrBlock<Range*>& yout,
                                                      Int na,
                                                      Int nb,
                                                      Int nc,
                                                      const Vector<Domain>& xout,
                                                      const Vector<Domain>& xin,
                                                      const PtrBlock<const Range*>& yin,
                                                      Int method)
{
  uInt nElements=xin.nelements();
  AlwaysAssert (nElements>0, AipsError);
  Domain x_req;
  switch (method) {
  case nearestNeighbour: // This does nearest neighbour interpolation
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=nearestNeigbour is not implemented yet"));
      return;
    }
  case linear: // Linear interpolation is the default
    {
      Int h;
      Int nxout=xout.nelements();
      for (Int j=0; j<nxout; j++) {
        x_req=xout[j];
        Bool found;
        uInt where = binarySearchBrackets(found, xin, x_req, nElements);
        if (where == nElements)
          where--;
        else if (where == 0)
          where++;
        Domain x2 = xin[where]; Int ind2 = where;
        where--;
        Domain x1 = xin[where]; Int ind1 = where;
        if (nearAbs(x1, x2))
          throw(AipsError("Interpolate1D::operator()"
                          " data has repeated x values"));
        Domain frac=(x_req-x1)/(x2-x1);
        for (Int k=0; k<nc; k++) {
          for (Int i=0; i<na; i++) {
        // column major
             h = i + j*na + k*na*nxout;
             Int xind1 = i + ind1*na + k*na*nb;
             Int xind2 = i + ind2*na + k*na*nb;
             yout[h][0] = yin[xind1][0] + frac * (yin[xind2][0] - yin[xind1][0]);
        //    return y1 + ((x_req-x1)/(x2-x1)) * (y2-y1);
          }
        }
      }
      return;
    }
  case cubic:// fit a cubic polynomial to the four nearest points
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=cubic is not implemented yet"));
      return;
    }
  case spline: // natural cubic splines
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=spline is not implemented"));
      return;
    }
  }
}

template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::interpolateyPtr(PtrBlock<Range*>& yout,
                                                      PtrBlock<Bool*>& youtFlags,
                                                      Int na,
                                                      Int nb,
                                                      Int nc,
                                                      const Vector<Domain>& xout,
                                                      const Vector<Domain>& xin,
                                                      const PtrBlock<const Range*>& yin,
                                                      const PtrBlock<const Bool*>& yinFlags,
                                                      Int method,
                                                      Bool goodIsTrue,
                                                      Bool extrapolate)
{
  uInt nElements=xin.nelements();
  Domain x_req;
  Bool flag = !(goodIsTrue);
  switch (method) {
  case nearestNeighbour: // This does nearest neighbour interpolation
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=nearestNeigbour is not implemented yet"));
      return;
    }
  case linear: // Linear interpolation is the default
    {
      Int h;
      Int nxout=xout.nelements();
      for (Int j=0; j<nxout; j++) {
        x_req=xout[j];
        Bool found;
        Bool discard = False;
        uInt where = binarySearchBrackets(found, xin, x_req, nElements);
        if (where == nElements) {
          discard=!extrapolate;
          where--;
        }
        else if (where == 0) {
          discard=(x_req!=xin[0])&&(!extrapolate);
          where++;
        }
        Domain x2 = xin[where]; Int ind2 = where;
        where--;
        Domain x1 = xin[where]; Int ind1 = where;
        if (nearAbs(x1, x2))
          throw(AipsError("Interpolate1D::operator()"
                          " data has repeated x values"));
        Domain frac=(x_req-x1)/(x2-x1);

//    y1 + ((x_req-x1)/(x2-x1)) * (y2-y1);
        if (goodIsTrue) {
          for (Int k=0; k<nc; k++) {
            for (Int i=0; i<na; i++) {
              // column major
              h = i + j*na + k*na*nxout;
              Int xind1 = i + ind1*na + k*na*nb;
              Int xind2 = i + ind2*na + k*na*nb;
              yout[h][0] = yin[xind1][0] + frac * (yin[xind2][0] - yin[xind1][0]);
              youtFlags[h][0] = (discard ? flag :
                                yinFlags[xind1][0] && yinFlags[xind2][0]);
            }
          }
        } else {
          for (Int k=0; k<nc; k++) {
            for (Int i=0; i<na; i++) {
              h = i + j*na + k*na*nxout;
              Int xind1 = i + ind1*na + k*na*nb;
              Int xind2 = i + ind2*na + k*na*nb;
             yout[h][0] = yin[xind1][0] + frac * (yin[xind2][0] - yin[xind1][0]);
             youtFlags[h][0] = ( discard ? flag :
                                 yinFlags[xind1][0] || yinFlags[xind2][0]);
            }
          }
        }
      }
      return ;
    }
  case cubic:// fit a cubic polynomial to the four nearest points
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=cubic is not implemented yet"));
      return;
    }
  case spline: // natural cubic splines
    {
      throw(AipsError("Interpolate1DArray::interpolateyPtr(): method=spline is not implemented"));
      return;
    }
  }
}




// Interpolate the y-vectors of length ny from x values xin to xout
// using polynomial interpolation with specified order
template <class Domain, class Range>
void InterpolateArray1D<Domain,Range>::polynomialInterpolation
(PtrBlock<Range*>& yout,
 Int ny, 
 const Vector<Domain>& xout, 
 const Vector<Domain>& xin,
 const PtrBlock<const Range*>& yin, 
 Int order)
{
  // Based on Nevilles Algorithm (Numerical Recipies 2nd ed., Section 3.1)
  // x is the point we want to estimate, n is the number of points to use
  // in the interpolation, and offset controls which n points are used
  // (normally the nearest points)
  
  // n = #points used in interpolation
  Int n = order+1;
  Block<Range> c(n), d(n);
  Block<Domain> x(n);
  Int nElements = xin.nelements();
  DebugAssert((n<=nElements),AipsError);
  for (Int i=0; i<Int(xout.nelements()); i++) {
    Domain x_req=xout[i];
    Bool found;
    Int where = binarySearchBrackets(found, xin, x_req, nElements);
    if (where > 1 && where < nElements - 1)
      where  = where - n/2;
    else if (where <= 1)
      where = 0;
    else
      where = nElements - n;
    
    for (Int j=0; j<ny; j++) {
      Int offset=where;
      // copy the x, y data into the working arrays
      for (Int i2 = 0; i2 < n; i2++){
	d[i2] = c[i2] = yin[offset][j]; 
	x[i2] = xin[offset];
	offset++;
      }
      // Now do the interpolation using the rather opaque algorithm
      Range w, y;
      y = c[0];
      const Float one = 1;
      for (Int k = 1; k < n; k++){
	// Calculate new C's and D's for each iteration 
	for (Int l = 0; l < n-k; l++){
	  if (nearAbs(x[l+k], x[l])) 
	    throw(AipsError("Interpolate1D::polynomialInterpolation"
			    " data has repeated x values"));
	  w = (c[l+1] - d[l]) * (one / (x[l] - x[l+k]));
	  c[l] = (x[l] - x_req) * w;
	  d[l] = (x[l+k] - x_req) * w;
	}
	y += c[0];
      }
      yout[i][j]=y;
    }
  }
}

} //# NAMESPACE CASACORE - END


#endif
