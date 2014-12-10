//# VanVleck.cc:  this implements VanVleck.
//# Copyright (C) 2002
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

//# Includes

#include <casacore/scimath/Mathematics/VanVleck.h>

#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// initial values for the static data members

Interpolate1D<Double, Double> *VanVleck::itsInterp = NULL;
uInt VanVleck::itsSize = 65;
uInt VanVleck::itsNx = 0;
uInt VanVleck::itsNy = 0;
Bool VanVleck::itsEquiSpaced = False;
Vector<Double> VanVleck::itsQx0;
Vector<Double> VanVleck::itsQx1;
Vector<Double> VanVleck::itsQy0;
Vector<Double> VanVleck::itsQy1;
Vector<Double> VanVleck::itsQx0Qx0;
Vector<Double> VanVleck::itsQy0Qy0;
Matrix<Double> VanVleck::itsQx0Qy0;
Matrix<Double> VanVleck::itsQx1Qy1diffs;
Double VanVleck::itsXlev = 0.0;
Double VanVleck::itsYlev = 0.0;
Double VanVleck::itsXmean = 0.0;
Double VanVleck::itsYmean = 0.0;
Mutex VanVleck::theirMutex;

#define NEED_UNDERSCORES
#if defined(NEED_UNDERSCORES)
#define dqags dqags_
#define vvr3 vvr3_
#define vvr9 vvr9_
#define vvr3auto vvr3auto_
#define vvr9auto vvr9auto_
#define vvr3zmean vvr3zmean_
#define vvr9zmean vvr9zmean_
#define vvr3zauto vvr3zauto_
#define vvr9zauto vvr9zauto_
#endif

extern "C" { 
   void dqags(Double (*)(Double *), Double*, Double *, Double *, Double *, Double *,
	      Double *, Int *, Int*, Int *, Int *, Int *, Int *, Double *);
}

extern "C" { 
   Double vvr3(Double*, Double *, Double *, Double *, Double *);
}

extern "C" { 
   Double vvr9(Double*, Double *, Double *, Double *, Double *);
}

extern "C" { 
   Double vvr3auto(Double*, Double *, Double *);
}

extern "C" { 
   Double vvr9auto(Double*, Double *, Double *);
}

extern "C" { 
   Double vvr3zmean(Double*, Double *, Double *);
}

extern "C" { 
   Double vvr9zmean(Double*, Double *, Double *);
}

extern "C" { 
   Double vvr3zauto(Double*, Double *);
}

extern "C" { 
   Double vvr9zauto(Double*, Double *);
}



void VanVleck::size(uInt npts)
{
    ScopedMutexLock lock(theirMutex);
    if (itsSize != npts) {
	itsSize = npts;
	initInterpolator();
    }
}

uInt VanVleck::getsize()
{
    return itsSize;
}

void VanVleck::setQuantization(const Matrix<Double> &qx, 
			       const Matrix<Double> &qy)
{
    ScopedMutexLock lock(theirMutex);
    // should double check that first dimension is 2

    uInt nx = qx.ncolumn();
    uInt ny = qy.ncolumn();
    Bool nxChanged = itsNx != nx;
    Bool nyChanged = itsNy != ny;

    if (nxChanged) {
	itsQx0.resize(nx);
	itsQx1.resize(nx);
	itsQx0Qx0.resize(nx);
	itsNx = nx;
    }
    if (nyChanged) {
	itsQy0.resize(ny);
	itsQy1.resize(ny);
	itsQy0Qy0.resize(ny);
	itsNy = ny;
    }
    if (nxChanged || nyChanged) {
	itsQx0Qy0.resize(nx,ny);
	itsQx1Qy1diffs.resize(nx,ny);
    }
    itsQx0 = qx.row(0);
    itsQx1 = qx.row(1);
    itsQy0 = qy.row(0);
    itsQy1 = qy.row(1);
    for (uInt i=0;i<itsNx;i++) {
	itsQx0Qx0[i] = -.5*itsQx0[i]*itsQx0[i];
	Double a = itsQx1[i+1]-itsQx1[i];
	for (uInt j=0;j<itsNy;j++) {
	    itsQx0Qy0(i,j) = itsQx0[i]*itsQy0[j];
	    itsQx1Qy1diffs(i,j) = a*(itsQy1[j+1]-itsQy1[j]);
	}
    }
    for (uInt j=0;j<itsNy;j++) {
	itsQy0Qy0[j] = -.5*itsQy0[j]*itsQy0[j];
    }
    initInterpolator();
}

Bool VanVleck::setEquiSpaced(Double xlev, Double ylev,
			     Double xmean, Double ymean,
			     Int n)
{
    Bool result = n==3 || n==9;
    if (result) {
        ScopedMutexLock lock(theirMutex);
	itsNx = itsNy = n;
	itsXlev = xlev;
	itsYlev = ylev;
	itsXmean = xmean;
	itsYmean = ymean;
	itsEquiSpaced = True;
	initInterpolator();
    }
    return result;
}


void VanVleck::initInterpolator()
{
  delete itsInterp;
  itsInterp = 0;

  Vector<Double> rs(itsSize);
  Vector<Double> rhos(itsSize);

  Double twoN = 2.0*itsSize;
  Double denom = cos(C::pi/twoN);
  Int midi = (itsSize-1)/2;
  rhos[midi] = 0.0;
  rs[midi] = 0.0;

  if (!itsEquiSpaced) {
      if (itsQx0.nelements() == 0) return;

      for (Int i=1;i<=midi;i++) {
	  // for the rhos, choose the modified Chebyshev points
	  // upper side
	  Int hi = midi+i;
	  rhos[hi] = -cos(Double(2*hi+1)*C::pi/twoN)/denom;
	  rs[hi] = rs[hi-1] + rinc(rhos[hi-1],rhos[hi]);
	  // lower side
	  Int lo = midi-i;
	  rhos[lo] = -cos(Double(2*lo+1)*C::pi/twoN)/denom;
	  rs[lo] = rs[lo+1] + rinc(rhos[lo+1],rhos[lo]);
      }
  } else {
      for (Int i=1;i<=midi;i++) {
	  // for the rhos, choose the modified Chebyshev points
	  // upper side
	  Int hi = midi+i;
	  rhos[hi] = -cos(Double(2*hi+1)*C::pi/twoN)/denom;
	  // lower side
	  Int lo = midi-i;
	  rhos[lo] = -cos(Double(2*lo+1)*C::pi/twoN)/denom;
      }
      if (nearAbs(itsXlev, itsYlev)) {
	  // auto-correlation
	  if (nearAbs(itsXmean, 0.0) && nearAbs(itsYmean, 0.0)) {
	      // zero-mean
	      // these are symetric about the mid-point
	      if (itsNx == 3) {
		  for (Int i=1;i<=midi;i++) {
		      Int hi = midi+i;
		      Int lo = midi-i;
		      rs[hi] = vvr3zauto(&itsXlev, &(rhos[hi]));
		      rs[lo] = -rs[hi];
		  }
	      } else {
		  // it must be 9
		  for (Int i=1;i<=midi;i++) {
		      Int hi = midi+i;
		      Int lo = midi-i;
		      rs[hi] = vvr9zauto(&itsXlev, &(rhos[hi]));
		      rs[lo] = -rs[hi];
		  }
	      }
	  } else {
	      if (itsNx == 3) {
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3auto(&itsXmean, &itsXlev, &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr9auto(&itsXmean, &itsXlev, &(rhos[i]));
		  }
	      }
	  }
      } else {
	  // cross-correlation
	  if (nearAbs(itsXmean, 0.0) && nearAbs(itsYmean, 0.0)) {
	      // zero-mean
	      if (itsNx == 3) {
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3zmean(&itsXlev, &itsYlev, &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr9zmean(&itsXlev, &itsYlev, &(rhos[i]));
		  }
	      }
	  } else {
	      if (itsNx == 3) {
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3(&itsXmean, &itsYmean, &itsXlev, &itsYlev, 
				   &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uInt i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr9(&itsXmean, &itsYmean, &itsXlev, &itsYlev, 
				   &(rhos[i]));
		  }
	      }
	  }
      }
  }
  // watch for repeat values - happens in really bad cases, but if it
  // this isn't done, Interpolate1D throws an exception.  We can do
  // this here and turn off the check there - so there shouldn't be
  // any additional cost here unless the data is bad and it has to be
  // decreased in size.
  uInt nels = rs.nelements();
  uInt i = 0;
  while (i<(nels-1)) {
      if (nearAbs(rs[i], rs[i+1])) {
	  // find the next value that isn't a duplicate
	  uInt ndrop=1;
	  while(ndrop<(nels-i-1) && nearAbs(rs[i],rs[i+1+ndrop])) ndrop++;
	  // slide everything to the lower value
	  nels -= ndrop;
	  uInt j = i;
	  while (j<(nels-1)) {
	      rs[j+1] = rs[j+1+ndrop];
	      rhos[j+1] = rhos[j+1+ndrop];
	      j++;
	  }
      }
      i++;
  }
  if (nels != rs.nelements()) {
      rs.resize(nels,True);
      rhos.resize(nels,True);
  }
  ScalarSampledFunctional<Double> fx(rs);
  ScalarSampledFunctional<Double> fy(rhos);
  itsInterp = new Interpolate1D<Double,Double>(fx, fy, True, True);
  AlwaysAssert(itsInterp, AipsError);
  itsInterp->setMethod(Interpolate1D<Double,Double>::spline);
}

void VanVleck::getTable(Vector<Double> &rs,
			Vector<Double> &rhos)
{
  ScopedMutexLock lock(theirMutex);
  rs.resize(itsInterp->getX().nelements());
  rs = itsInterp->getX();
  rhos.resize(itsInterp->getY().nelements());
  rhos = itsInterp->getY();
}

Double VanVleck::r(const Double rho)
{
  ScopedMutexLock lock(theirMutex);
  return (*itsInterp)(rho);
}

Bool VanVleck::dcoff(Double &dcoffset, Double &threshold,
		     Int n, Double zerolag, Double bias)
{
    Bool result = True;
    if (n == 3) {
	result = dcoff3(dcoffset, threshold, zerolag, bias);
    } else {
	dcoffset = 0.0;
	threshold = thresh(n,zerolag);
    }
    return result;
}


// Only private functions hereafter. They do not need to be locked.
double VanVleck::drbydrho(double *rho)
{
    Double s = 0.0;
    Double thisRho = *rho;
    Double oneMinusRhoRho = 1.0 - thisRho*thisRho;
    Double denom = C::_2pi*sqrt(oneMinusRhoRho);

    for (uInt i=0;i<(itsNx-1);i++) {
	for (uInt j=0;j<(itsNy-1);j++) {
	    s+=itsQx1Qy1diffs(i,j) *
		exp((itsQx0Qx0[i]+thisRho*itsQx0Qy0(i,j)+itsQy0Qy0[j])/oneMinusRhoRho) /
		denom;
	}
    }
    return s;
}

Double VanVleck::rinc(Double &rhoi, Double &rhof)
{
  Double work[4096];
  Int iwork[1024];
  Double result, abserr;
  Int neval, ier, last;

  Double epsabs=1.0e-6;
  Double epsrel=1.0e-6;
  Int limit=1024;
  Int lenw = 4*limit;
  dqags(drbydrho, &rhoi, &rhof, &epsabs, &epsrel, &result, &abserr,
	&neval, &ier, &limit, &lenw, &last, iwork, work);
  if (ier != 0) {
    cout << "Error in dqags : " << ier << endl;
  }
  return result;
}

Double VanVleck::threshNgt3(Int n, Double zerolag)
{
  Double x = 0.0;
  Bool odd = True;
  if (n%2 == 0) {
    x = 1.0;
    odd = False;
  }
  Double tol = 1.0e-8;
  Double sqrt2 = sqrt(2.0);
  Double sqrt2dpi = sqrt(2.0/C::pi);
  Double fp, f;
  for (Int i=0;i<30;i++) {
    fp = 0.0;
    f = zerolag;
    if (odd) {
      for (Int k=1;k<=(n-1)/2;k++) {
	f -= (2*k-1)*::erfc((2*k-1)*x/sqrt2);
	Double twoKm1 = 2*k-1;
	fp += sqrt2dpi*twoKm1*twoKm1*exp(-0.5*(twoKm1*x)*(twoKm1*x));
      }
    } else {
      f -= 1.0;
      for (Int k=1;k<=(n-2)/2;k++) {
	f -= 8*k*::erfc(k*x/sqrt2);
	fp += 8*k*k*sqrt2dpi*exp(-0.5*(k*x)*(k*x));
      }
    }
    Double deltax = -f/fp;
    Double signdx = (deltax>=0) ? 1.0 : -1.0;
    deltax = signdx * min(0.5,abs(deltax));
    x += deltax;
    if (odd) x = max(0.0, x);
    if (abs(deltax/x) < tol) break;
  }
  return x;
}

Double VanVleck::invErf(Double x)
{
  // these are translations of Mathematic code supplied by Fred Schwab
  // based upon approximations published by Blair, Edwards, and Johnson.
 
  Double absx = abs(x);
  Double result;
  if (absx<=0.75) {
    // from table 10 of Blair et. al.
    // maximum relative error of 4.47e-8
    Double t = x*x-0.75*0.75;
    Double p1, p2, p3, q1, q2, q3, q4;
    p1 = -13.0959967422;
    p2 =  26.785225760;
    p3 =  -9.289057635;
    q1 = -12.0749426297;
    q2 =  30.960614529;
    q3 = -17.149977991;
    q4 =   1.0;
    result = x*(p1+t*(p2+t*p3))/(q1+t*(q2+t*(q3+t*q4)));
  } else if (absx<=0.9375) {
    // from table 29 of Blair et. al.
    // maximum relative error of 4.17e-8
    Double t = x*x-.9375*.9375;
    Double p1,p2,p3,p4,q1,q2,q3,q4;
    p1 = -0.12402565221;
    p2 =  1.0688059574;
    p3 = -1.9594556078;
    p4 =  0.4230581357;
    q1 = -0.8827697997;
    q2 =  0.8900743359;
    q3 = -2.1757031196;
    q4 =  1.0;
    result = x*(p1+t*(p2+t*(p3+t*p4)))/(q1+t*(q2+t*(q3+t*q4)));
  } else if (absx<(1-1e-100)) {
    // from table 50 of Blair et. al.
    // maximum relative error of 2.45e-8
    Double t = 1.0/sqrt(-log(1.0-absx));
    Double p1,p2,p3,p4,p5,p6,q1,q2,q3;
    p1 =  0.1550470003116;
    p2 =  1.382719649631;
    p3 =  0.690969348887;
    p4 = -1.128081391617;
    p5 =  0.680544246825;
    p6 = -0.16444156791;
    q1 = 0.155024849822;
    q2 = 1.385228141995;
    q3 = 1.0;
    Double signx = (x>=0) ? 1.0 : -1.0;
    result = signx*(p1/t+p2+t*(p3+t*(p4+t*(p5+t*p6))))/(q1+t*(q2+t*q3));
  } else {
    result = C::dbl_max;
    if (x<0) {
      result = -result;
    }
  }
  return result;
}

Double VanVleck::invErfc(Double x)
{
  Double result;
  if (x>=2.0) {
    result = -C::dbl_max;
  } else if (x>=0.0625) {
    // just use invErf(1-x)
    result = invErf(1.0-x);
  } else if (x>=1e-100) {
    // From table 50 of Blair et al as well as table 70
    Double t = 1.0/sqrt(-log(x));
    Double p1,p2,p3,p4,p5,p6;
    Double q1,q2,q3;
    p1 =  0.1550470003116;
    p2 =  1.382719649631;
    p3 =  0.690969348887;
    p4 = -1.128081391617;
    p5 =  0.680544246825;
    p6 = -0.16444156791;
    q1 = 0.155024849822;
    q2 = 1.385228141995;
    q3 = 1.0;
    result = (p1/t+p2+t*(p3+t*(p4+t*(p5+t*p6)))) / (q1+t*(q2+t*q3));
  } else if (x>0) {
    // from table 70 of Blair et al
    // maximum relative error of 2.45e-8
    Double t = 1.0/sqrt(-log(x));
    Double p1,p2,p3,p4;
    Double q1,q2,q3;
    p1 = 0.00980456202915;
    p2 = 0.363667889171;
    p3 = 0.97302949837;
    p4 = -0.5374947401;
    q1 = 0.00980451277802;
    q2 = 0.363699971544;
    q3 = 1.0;
    result = (p1/t+p2+t*(p3+t*p4))/(q1+t*(q2+t*q3));
  } else {
    result = C::dbl_max;
  }
  return result;
}

Double VanVleck::predictNgt3(Int n, Double threshhold)
{
  Double result = 0.0;
  if (n%2 == 0) {
    // even n
    for (Int k=1;k<=(n-2)/2;k++) {
      result += ::erfc(k*threshhold/sqrt(2.0));
    }
    result = 1.0 + 8.0*result;
  } else {
    // odd n
    for (Int k=1;k<=(n-1)/2;k++) {
      result += (2*k-1)*::erfc((2*k-1)*threshhold/sqrt(2.0));
    }
  }
  return result;
}


Bool VanVleck::dcoff3(Double &dcoffset, Double &threshold,
		      Double zerolag, Double bias)
{
    // the input data, bias and zerolag, should satisfy the
    // inequality constraints 0 <= bias < 1 and
    // sqrt(bias) < zerolag < 2-sqrt(bias)
 
    Bool result = True;
    Double rtbias = sqrt(bias);
    if (bias < 0.0 || bias >= 1.0 || rtbias >= zerolag ||
	zerolag >= (2.0-rtbias)) {
	// fall back and return False
	result = False;
	dcoffset = 0.0;
	threshold = threshN3(zerolag);
    } else {
	Double rt2 = sqrt(2.0);
	Double t1 = invErf(1.0+rtbias-zerolag);
	Double t2 = invErf(-1.0+rtbias+zerolag);
	dcoffset = (t1+t2)/rt2;
	threshold = (t1-t2)/rt2;
    }
    return result;
}

} //# NAMESPACE CASACORE - END

