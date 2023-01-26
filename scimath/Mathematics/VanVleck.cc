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

//# Includes

#include <casacore/scimath/Mathematics/VanVleck.h>

#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// initial values for the static data members

Interpolate1D<double, double> *VanVleck::itsInterp = NULL;
uint32_t VanVleck::itsSize = 65;
uint32_t VanVleck::itsNx = 0;
uint32_t VanVleck::itsNy = 0;
bool VanVleck::itsEquiSpaced = false;
Vector<double> VanVleck::itsQx0;
Vector<double> VanVleck::itsQx1;
Vector<double> VanVleck::itsQy0;
Vector<double> VanVleck::itsQy1;
Vector<double> VanVleck::itsQx0Qx0;
Vector<double> VanVleck::itsQy0Qy0;
Matrix<double> VanVleck::itsQx0Qy0;
Matrix<double> VanVleck::itsQx1Qy1diffs;
double VanVleck::itsXlev = 0.0;
double VanVleck::itsYlev = 0.0;
double VanVleck::itsXmean = 0.0;
double VanVleck::itsYmean = 0.0;
std::mutex VanVleck::theirMutex;

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
   void dqags(double (*)(double *), double*, double *, double *, double *, double *,
	      double *, int32_t *, int32_t*, int32_t *, int32_t *, int32_t *, int32_t *, double *);
}

extern "C" { 
   double vvr3(double*, double *, double *, double *, double *);
}

extern "C" { 
   double vvr9(double*, double *, double *, double *, double *);
}

extern "C" { 
   double vvr3auto(double*, double *, double *);
}

extern "C" { 
   double vvr9auto(double*, double *, double *);
}

extern "C" { 
   double vvr3zmean(double*, double *, double *);
}

extern "C" { 
   double vvr9zmean(double*, double *, double *);
}

extern "C" { 
   double vvr3zauto(double*, double *);
}

extern "C" { 
   double vvr9zauto(double*, double *);
}



void VanVleck::size(uint32_t npts)
{
    std::lock_guard<std::mutex> lock(theirMutex);
    if (itsSize != npts) {
	itsSize = npts;
	initInterpolator();
    }
}

uint32_t VanVleck::getsize()
{
    return itsSize;
}

void VanVleck::setQuantization(const Matrix<double> &qx, 
			       const Matrix<double> &qy)
{
    std::lock_guard<std::mutex> lock(theirMutex);
    // should double check that first dimension is 2

    uint32_t nx = qx.ncolumn();
    uint32_t ny = qy.ncolumn();
    bool nxChanged = itsNx != nx;
    bool nyChanged = itsNy != ny;

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
    for (uint32_t i=0;i<itsNx;i++) {
	itsQx0Qx0[i] = -.5*itsQx0[i]*itsQx0[i];
	double a = itsQx1[i+1]-itsQx1[i];
	for (uint32_t j=0;j<itsNy;j++) {
	    itsQx0Qy0(i,j) = itsQx0[i]*itsQy0[j];
	    itsQx1Qy1diffs(i,j) = a*(itsQy1[j+1]-itsQy1[j]);
	}
    }
    for (uint32_t j=0;j<itsNy;j++) {
	itsQy0Qy0[j] = -.5*itsQy0[j]*itsQy0[j];
    }
    initInterpolator();
}

bool VanVleck::setEquiSpaced(double xlev, double ylev,
			     double xmean, double ymean,
			     int32_t n)
{
    bool result = n==3 || n==9;
    if (result) {
        std::lock_guard<std::mutex> lock(theirMutex);
	itsNx = itsNy = n;
	itsXlev = xlev;
	itsYlev = ylev;
	itsXmean = xmean;
	itsYmean = ymean;
	itsEquiSpaced = true;
	initInterpolator();
    }
    return result;
}


void VanVleck::initInterpolator()
{
  delete itsInterp;
  itsInterp = 0;

  Vector<double> rs(itsSize);
  Vector<double> rhos(itsSize);

  double twoN = 2.0*itsSize;
  double denom = cos(C::pi/twoN);
  int32_t midi = (itsSize-1)/2;
  rhos[midi] = 0.0;
  rs[midi] = 0.0;

  if (!itsEquiSpaced) {
      if (itsQx0.nelements() == 0) return;

      for (int32_t i=1;i<=midi;i++) {
	  // for the rhos, choose the modified Chebyshev points
	  // upper side
	  int32_t hi = midi+i;
	  rhos[hi] = -cos(double(2*hi+1)*C::pi/twoN)/denom;
	  rs[hi] = rs[hi-1] + rinc(rhos[hi-1],rhos[hi]);
	  // lower side
	  int32_t lo = midi-i;
	  rhos[lo] = -cos(double(2*lo+1)*C::pi/twoN)/denom;
	  rs[lo] = rs[lo+1] + rinc(rhos[lo+1],rhos[lo]);
      }
  } else {
      for (int32_t i=1;i<=midi;i++) {
	  // for the rhos, choose the modified Chebyshev points
	  // upper side
	  int32_t hi = midi+i;
	  rhos[hi] = -cos(double(2*hi+1)*C::pi/twoN)/denom;
	  // lower side
	  int32_t lo = midi-i;
	  rhos[lo] = -cos(double(2*lo+1)*C::pi/twoN)/denom;
      }
      if (nearAbs(itsXlev, itsYlev)) {
	  // auto-correlation
	  if (nearAbs(itsXmean, 0.0) && nearAbs(itsYmean, 0.0)) {
	      // zero-mean
	      // these are symetric about the mid-point
	      if (itsNx == 3) {
		  for (int32_t i=1;i<=midi;i++) {
		      int32_t hi = midi+i;
		      int32_t lo = midi-i;
		      rs[hi] = vvr3zauto(&itsXlev, &(rhos[hi]));
		      rs[lo] = -rs[hi];
		  }
	      } else {
		  // it must be 9
		  for (int32_t i=1;i<=midi;i++) {
		      int32_t hi = midi+i;
		      int32_t lo = midi-i;
		      rs[hi] = vvr9zauto(&itsXlev, &(rhos[hi]));
		      rs[lo] = -rs[hi];
		  }
	      }
	  } else {
	      if (itsNx == 3) {
		  for (uint32_t i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3auto(&itsXmean, &itsXlev, &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uint32_t i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr9auto(&itsXmean, &itsXlev, &(rhos[i]));
		  }
	      }
	  }
      } else {
	  // cross-correlation
	  if (nearAbs(itsXmean, 0.0) && nearAbs(itsYmean, 0.0)) {
	      // zero-mean
	      if (itsNx == 3) {
		  for (uint32_t i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3zmean(&itsXlev, &itsYlev, &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uint32_t i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr9zmean(&itsXlev, &itsYlev, &(rhos[i]));
		  }
	      }
	  } else {
	      if (itsNx == 3) {
		  for (uint32_t i=0;i<rhos.nelements();i++) {
		      rs[i] = vvr3(&itsXmean, &itsYmean, &itsXlev, &itsYlev, 
				   &(rhos[i]));
		  }
	      } else {
		  // it must be 9
		  for (uint32_t i=0;i<rhos.nelements();i++) {
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
  uint32_t nels = rs.nelements();
  uint32_t i = 0;
  while (i<(nels-1)) {
      if (nearAbs(rs[i], rs[i+1])) {
	  // find the next value that isn't a duplicate
	  uint32_t ndrop=1;
	  while(ndrop<(nels-i-1) && nearAbs(rs[i],rs[i+1+ndrop])) ndrop++;
	  // slide everything to the lower value
	  nels -= ndrop;
	  uint32_t j = i;
	  while (j<(nels-1)) {
	      rs[j+1] = rs[j+1+ndrop];
	      rhos[j+1] = rhos[j+1+ndrop];
	      j++;
	  }
      }
      i++;
  }
  if (nels != rs.nelements()) {
      rs.resize(nels,true);
      rhos.resize(nels,true);
  }
  ScalarSampledFunctional<double> fx(rs);
  ScalarSampledFunctional<double> fy(rhos);
  itsInterp = new Interpolate1D<double,double>(fx, fy, true, true);
  AlwaysAssert(itsInterp, AipsError);
  itsInterp->setMethod(Interpolate1D<double,double>::spline);
}

void VanVleck::getTable(Vector<double> &rs,
			Vector<double> &rhos)
{
  std::lock_guard<std::mutex> lock(theirMutex);
  rs.resize(itsInterp->getX().nelements());
  rs = itsInterp->getX();
  rhos.resize(itsInterp->getY().nelements());
  rhos = itsInterp->getY();
}

double VanVleck::r(const double rho)
{
  std::lock_guard<std::mutex> lock(theirMutex);
  return (*itsInterp)(rho);
}

bool VanVleck::dcoff(double &dcoffset, double &threshold,
		     int32_t n, double zerolag, double bias)
{
    bool result = true;
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
    double s = 0.0;
    double thisRho = *rho;
    double oneMinusRhoRho = 1.0 - thisRho*thisRho;
    double denom = C::_2pi*sqrt(oneMinusRhoRho);

    for (uint32_t i=0;i<(itsNx-1);i++) {
	for (uint32_t j=0;j<(itsNy-1);j++) {
	    s+=itsQx1Qy1diffs(i,j) *
		exp((itsQx0Qx0[i]+thisRho*itsQx0Qy0(i,j)+itsQy0Qy0[j])/oneMinusRhoRho) /
		denom;
	}
    }
    return s;
}

double VanVleck::rinc(double &rhoi, double &rhof)
{
  double work[4096];
  int32_t iwork[1024];
  double result, abserr;
  int32_t neval, ier, last;

  double epsabs=1.0e-6;
  double epsrel=1.0e-6;
  int32_t limit=1024;
  int32_t lenw = 4*limit;
  dqags(drbydrho, &rhoi, &rhof, &epsabs, &epsrel, &result, &abserr,
	&neval, &ier, &limit, &lenw, &last, iwork, work);
  if (ier != 0) {
    cout << "Error in dqags : " << ier << endl;
  }
  return result;
}

double VanVleck::threshNgt3(int32_t n, double zerolag)
{
  double x = 0.0;
  bool odd = true;
  if (n%2 == 0) {
    x = 1.0;
    odd = false;
  }
  double tol = 1.0e-8;
  double sqrt2 = sqrt(2.0);
  double sqrt2dpi = sqrt(2.0/C::pi);
  double fp, f;
  for (int32_t i=0;i<30;i++) {
    fp = 0.0;
    f = zerolag;
    if (odd) {
      for (int32_t k=1;k<=(n-1)/2;k++) {
	f -= (2*k-1)*::erfc((2*k-1)*x/sqrt2);
	double twoKm1 = 2*k-1;
	fp += sqrt2dpi*twoKm1*twoKm1*exp(-0.5*(twoKm1*x)*(twoKm1*x));
      }
    } else {
      f -= 1.0;
      for (int32_t k=1;k<=(n-2)/2;k++) {
	f -= 8*k*::erfc(k*x/sqrt2);
	fp += 8*k*k*sqrt2dpi*exp(-0.5*(k*x)*(k*x));
      }
    }
    double deltax = -f/fp;
    double signdx = (deltax>=0) ? 1.0 : -1.0;
    deltax = signdx * min(0.5,abs(deltax));
    x += deltax;
    if (odd) x = max(0.0, x);
    if (abs(deltax/x) < tol) break;
  }
  return x;
}

double VanVleck::invErf(double x)
{
  // these are translations of Mathematic code supplied by Fred Schwab
  // based upon approximations published by Blair, Edwards, and Johnson.
 
  double absx = abs(x);
  double result;
  if (absx<=0.75) {
    // from table 10 of Blair et. al.
    // maximum relative error of 4.47e-8
    double t = x*x-0.75*0.75;
    double p1, p2, p3, q1, q2, q3, q4;
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
    double t = x*x-.9375*.9375;
    double p1,p2,p3,p4,q1,q2,q3,q4;
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
    double t = 1.0/sqrt(-log(1.0-absx));
    double p1,p2,p3,p4,p5,p6,q1,q2,q3;
    p1 =  0.1550470003116;
    p2 =  1.382719649631;
    p3 =  0.690969348887;
    p4 = -1.128081391617;
    p5 =  0.680544246825;
    p6 = -0.16444156791;
    q1 = 0.155024849822;
    q2 = 1.385228141995;
    q3 = 1.0;
    double signx = (x>=0) ? 1.0 : -1.0;
    result = signx*(p1/t+p2+t*(p3+t*(p4+t*(p5+t*p6))))/(q1+t*(q2+t*q3));
  } else {
    result = C::dbl_max;
    if (x<0) {
      result = -result;
    }
  }
  return result;
}

double VanVleck::invErfc(double x)
{
  double result;
  if (x>=2.0) {
    result = -C::dbl_max;
  } else if (x>=0.0625) {
    // just use invErf(1-x)
    result = invErf(1.0-x);
  } else if (x>=1e-100) {
    // From table 50 of Blair et al as well as table 70
    double t = 1.0/sqrt(-log(x));
    double p1,p2,p3,p4,p5,p6;
    double q1,q2,q3;
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
    double t = 1.0/sqrt(-log(x));
    double p1,p2,p3,p4;
    double q1,q2,q3;
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

double VanVleck::predictNgt3(int32_t n, double threshhold)
{
  double result = 0.0;
  if (n%2 == 0) {
    // even n
    for (int32_t k=1;k<=(n-2)/2;k++) {
      result += ::erfc(k*threshhold/sqrt(2.0));
    }
    result = 1.0 + 8.0*result;
  } else {
    // odd n
    for (int32_t k=1;k<=(n-1)/2;k++) {
      result += (2*k-1)*::erfc((2*k-1)*threshhold/sqrt(2.0));
    }
  }
  return result;
}


bool VanVleck::dcoff3(double &dcoffset, double &threshold,
		      double zerolag, double bias)
{
    // the input data, bias and zerolag, should satisfy the
    // inequality constraints 0 <= bias < 1 and
    // sqrt(bias) < zerolag < 2-sqrt(bias)
 
    bool result = true;
    double rtbias = sqrt(bias);
    if (bias < 0.0 || bias >= 1.0 || rtbias >= zerolag ||
	zerolag >= (2.0-rtbias)) {
	// fall back and return false
	result = false;
	dcoffset = 0.0;
	threshold = threshN3(zerolag);
    } else {
	double rt2 = sqrt(2.0);
	double t1 = invErf(1.0+rtbias-zerolag);
	double t2 = invErf(-1.0+rtbias+zerolag);
	dcoffset = (t1+t2)/rt2;
	threshold = (t1-t2)/rt2;
    }
    return result;
}

} //# NAMESPACE CASACORE - END

