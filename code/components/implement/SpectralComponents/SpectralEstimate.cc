//# SpectralEstimate.cc: Get an initial estimate for spectral lines
//# Copyright (C) 2001
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
#include <trial/Wnbt/SpectralEstimate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <trial/Wnbt/SpectralElement.h>

//# Constructors
SpectralEstimate::SpectralEstimate() :
  useWindow_p(False), rms_p(0), cutoff_p(0),
  windowLow_p(0), windowEnd_p(0),
  q_p(2), sigmin_p(0),
  maxpar_p(200), deriv_p(0), pars_p(0), npar_p(0) {
  pars_p = new SpectralElement[maxpar_p];
}

SpectralEstimate::SpectralEstimate(const Double ampl,
				   const Double center, const Double sigma) {
  ///
}

SpectralEstimate::SpectralEstimate(const SpectralEstimate &other) {
  ///
}

SpectralEstimate::~SpectralEstimate() {
  delete [] deriv_p; deriv_p = 0;
  delete [] pars_p; pars_p = 0; npar_p = 0;
}

SpectralEstimate &SpectralEstimate::operator=(const SpectralEstimate &other) {
  if (this != &other) {
    ///
  };
  return *this;
}

uInt SpectralEstimate::estimate(const Vector<Float> &prof) {

  // Check if signal in window
  if (!window(prof)) return (0);
  // Limit window
  windowEnd_p = min(windowEnd_p+q_p , Int(prof.nelements())-1); 
  windowLow_p = max(windowLow_p-q_p , 0 );
  windowEnd_p = prof.nelements()-1;
  windowLow_p = 0;   
  // Get the second derivatives
  findc2(prof);
  // Find the estimates
  npar_p = findga(prof);
  ///  if (npar_p > 1) qsort( pars, npar_p, sizeof( par_struct ), compar );
  return (npar_p);
}

Int SpectralEstimate::compar(const SpectralElement &p1,
			     const SpectralElement &p2 ) {
   if (p1.getAmpl() > p2.getAmpl()) return (-1);
   else if (p1.getAmpl() < p2.getAmpl()) return (1);
   else return (0);
}

const SpectralElement SpectralEstimate::element(uInt which) const {
  return pars_p[which]; /// test which
}

uInt SpectralEstimate::window(const Vector<Float> &prof) {
  if (!useWindow_p || rms_p <= 0.0) {
    windowLow_p =0;
    windowEnd_p = prof.nelements();
    return prof.nelements();
  };
  if (prof.nelements() == 0) return 0;
  // Total flux in profile and max position
  Double flux(0.0);
  Double pmax(prof(0));
  uInt imax(0);
  for (uInt i=0; i<prof.nelements(); i++) {
    if (prof(i)>pmax) {
      pmax = prof(i);
      imax = i;
    };
    flux += prof(i);
  };
  // No data
  if (pmax < cutoff_p) return 0;
  // Window boundaries; new/old base and centre; width
  Int windowEnd_p;
  Int windowLow_p;
  Int width(0);
  Int nw(0);
  Double bnew(flux), bold;
  Double cnew(imax), cold;
  do {
    cold = cnew;
    bold = bnew;
    windowLow_p = max(0, Int(floor(cold-width++)));
    windowEnd_p = min(Int(prof.nelements()), Int(ceil(cold+width)));
    // flux and first moment in window
    Double s(0);
    Double c(0);
    for (Int i=windowLow_p; i<windowEnd_p; i++) {
      s += prof(i);
      c += i*prof(i);
    };
    bnew = flux-s;
    nw = prof.nelements()-windowEnd_p+windowLow_p-1;
    if (s != 0.0) {
      cnew = c/s;
      if (cnew < 0 || cnew > prof.nelements()) cnew = cold;
    };
  } while (abs(bnew-bold) > rms_p && nw);
  windowLow_p = windowLow_p;
  windowEnd_p = windowEnd_p;
  return nw;
}

void SpectralEstimate::findc2(const Vector<Float> &prof) {
  delete [] deriv_p; deriv_p = 0;
  deriv_p = new Double(prof.nelements());
  // Save the smoothing data
  static Int oldq = -1;
  static Double a, b;
  if (oldq != q_p) {
    a = 90.0/Double(q_p*(q_p+1)*(4*q_p*q_p-1)*(2*q_p+3));
    b = (q_p*(q_p+1))/3.0;
    oldq = q_p;
  };
  for (Int i=windowLow_p; i<windowEnd_p; i++) {
    // Moments
    Double m0(0.0); 
    Double m2(0.0); 
    for (Int j = -q_p; j < q_p; j++) {
      Int k = i+j;
      if (k >= 0 && k<Int(prof.nelements())) {
	// add to moments
	m0 += prof(k);
	m2 += prof(k)*j*j;
      };
    };
    // get the derivative
    deriv_p[i] = a*(m2-b*m0);
  };
}

Int SpectralEstimate::findga(const Vector<Float> &prof) {
  delete [] pars_p; pars_p = 0; npar_p = 0;
  pars_p = new SpectralElement[maxpar_p];
  Int i(windowLow_p-1);
  // Window on Gaussian
  Int iclo(windowLow_p);
  Int ichi;
  // Peak counter
  Int nmax = 0;
  Int r = 0;

  while (i++ < windowEnd_p) {
    if (deriv_p[i] > 0.0) {
      // At edge?
      if (i > windowLow_p && i < windowEnd_p) {
	if (deriv_p[i-1] < deriv_p[i] && deriv_p[i+1] < deriv_p[i]) {
	  // Peak in 2nd derivative
	  nmax += 1;
	};
      } else if (i == windowLow_p && deriv_p[i+1] < deriv_p[i]) {
	// At start
	nmax += 1;
      } else if (i == windowEnd_p && deriv_p[i-1] < deriv_p[i]) {
	// At end of window
	nmax += 1;
      };
    };
    switch (nmax) {
      // Search for next peak
    case 1:
      break;
      // Found a Gaussian
    case 2: {
      // Some moments
      Double m0m(0);
      Double m0(0);
      Double m1(0);
      Double m2(0);
      
      ichi = i;
      // Do Schwarz' calculation
      Double b = deriv_p[iclo];
      Double a = (deriv_p[ichi] - b) / Double(ichi - iclo);
      for (Int ic=iclo; ic<=ichi; ic++) {
	Double wi;
	m0m += min(deriv_p[ic], 0.0);
	wi = deriv_p[ic] - a*Double(ic-iclo) - b;
	m0 += wi;
	m1 += wi*ic;
	m2 += wi*ic*ic;
      };
      // determinant
      Double det = m2*m0 - m1*m1;
      if (det > 0.0 && fabs(m0m) >  FLT_EPSILON) {
	Double   xm = m1/m0;
	Double   yh, yl;
	
	Double sg = 1.69*sqrt(det) / fabs(m0);
	// Width above critical?
	if (sg > sigmin_p) {
	  Int is = Int(1.73*sg+0.5);
	  Int im = Int(xm+0.5);
	  if ((im-is) < 0) yl = 0.0;
	  else yl = prof(im-is);
	  Double ym = prof(im);
	  if ((im + is) > Int(prof.nelements()-1)) yh = 0.0;
	  else yh = prof(im+is);
	  Double pg = (ym-0.5*(yh+yl))/(1.0-exp(-0.5*((Double)(is*is))/sg/sg));
	  pg = min(pg, ym);
	  // Above critical level? Add to list
	  if (pg > cutoff_p) {
	    if (r < maxpar_p) {
	      pars_p[r].setAmpl(pg);
	      pars_p[r].setCenter(sg);
	      pars_p[r].setSigma(xm);
	    };
	    // Count Gaussians
	    r += 1;
	  };
	};
      };
      // Next gaussian
      iclo = ichi;
      nmax -= 1;
      break;
    }
    default: {
      iclo = i+1;
      break;
    }
    };
  };
  return (r);
}
