//# EarthField.cc:  EarthField class model calculations
//# Copyright (C) 1998-2000,2007
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
#include <casacore/measures/Measures/EarthField.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double EarthField::INTV = 50000;

//# Static data
uInt EarthField::interval_reg_p = 0;

//# Constructors
EarthField::EarthField() :
  method_p(EarthField::STANDARD), fixedEpoch_p(MeasData::MJD2000), agh_p(0), 
  p_p(0), q_p(0), cl_p(0), sl_p(0),
  lres_p(0) {
    fillField();
}

EarthField::EarthField(const EarthField &other) {
  copy(other);
}

EarthField::EarthField(EarthFieldTypes model, Double catepoch) :
  method_p(model), fixedEpoch_p(catepoch),
  p_p(0), q_p(0), cl_p(0), sl_p(0),
  lres_p(0) {
    fillField();
  }

EarthField &EarthField::operator=(const EarthField &other) {
  if ( this != &other) copy(other);
  return *this;
}

void EarthField::init() {
  method_p = EarthField::STANDARD;
  fixedEpoch_p = MeasData::MJD2000;
  fillField();
}

void EarthField::init(EarthFieldTypes model, Double catepoch) {
  method_p = model;
  fixedEpoch_p = catepoch;
  fillField();
}

//# Destructor
EarthField::~EarthField() {}

//# Operators
// Calculate EarthField components
const Vector<Double> &EarthField::operator()(const MVPosition &pos) {
  calcField(pos);
  Vector<Double> dx((pos-checkPos_p).getValue());
  lres_p++; lres_p %= 4;
  for (Int i=0; i<3; i++) {
    result_p[lres_p](i) = pval_p[i] +
      dx(0)*dval_p[0][i] + dx(1)*dval_p[1][i] + dx(2)*dval_p[2][i];
  }
  return result_p[lres_p];
}

//# Member functions
const Vector<Double> *EarthField::derivative(const MVPosition &pos) {
  calcField(pos);
  lres_p=0;		// Make sure contiguous set
  for (Int j=0; j<3; j++) {
    lres_p++; lres_p %= 4;
    for (Int i=0; i<3; i++) {
      result_p[lres_p](i) = dval_p[j][i];
    }
  }
  return &result_p[1];
}

void EarthField::copy(const EarthField &other) {
  method_p = other.method_p;
  fixedEpoch_p = other.fixedEpoch_p;
  agh_p = other.agh_p;
  checkPos_p = other.checkPos_p;
  for (Int i=0; i<3; i++) {
    pval_p[i] = other.pval_p[i];
    for (Int k=0; k<3; k++) dval_p[i][k] = other.dval_p[i][k];
  }
  for (Int j=0; j<4; j++) {
    result_p[j] = other.result_p[j];
  }
}

void EarthField::fillField() {

  // Get the interpolation interval
  if (!EarthField::interval_reg_p) {
    interval_reg_p = 
      AipsrcValue<Double>::registerRC(String("measures.earthfield.d_interval"),
				      Unit("km"), Unit("m"),
				      EarthField::INTV);
  }

  checkPos_p = MVPosition(1e30, 1e30, 1e30);
  switch (method_p) {
  default:
    agh_p.resize(0);
    agh_p =  MeasTable::IGRF(fixedEpoch_p);
    p_p.resize(PQ_LEN);
    q_p.resize(PQ_LEN);
    cl_p.resize(2*PQ_LEN);
    sl_p.resize(2*PQ_LEN);
    break;
  }
  for (Int j=0; j<4; j++) {
    result_p[j].resize(3);
    for (Int k=0; k<3; ++k) result_p[j][k] = 0;
  }
  for (Int j=0; j<3; ++j) {
    pval_p[j] = 0;
    for (Int k=0; k<3; ++k) dval_p[j][k] = 0;
  }
}

void EarthField::refresh() {
  fillField();
}

void EarthField::calcField(const MVPosition &pos) {
  if (!pos.nearAbs(checkPos_p,
		   AipsrcValue<Double>::get(EarthField::interval_reg_p))) {
    checkPos_p = pos;
    Vector<Double> posmv(3);
    posmv = pos.getValue();
    Vector<Double> posv(3);
    posv = pos.get();
    switch (method_p) {
    case NONE: {
      for (uInt j=0; j<3; j++) {
	pval_p[j] =0;
	for (uInt i=0; i<3; i++) dval_p[j][i] =0;
      }
    }
    break;
    default: {
      Double slat, clat, slong, clong, x, y, z, ratio, rr(0), one, two, three;
      Int l, m, n, fn(0), fm, j, i;
      for (Int lp=0; lp<4; lp++) {
	slat = cos(C::pi_2 - posv(2));
	clat = sin(C::pi_2 - posv(2));
	slong = sin(posv(1));
	clong = cos(posv(1));
	cl_p(0) = clong;
	sl_p(0) = slong;
	x = 0.0;
	y = 0.0;
	z = 0.0;
	l = 0;
	m = 0;
	n = 0;
	ratio = 6371200/posv(0);
	//
	// Compute Schmidt quasi-normal coefficients P and X (=Q)
	//
	p_p(0) = 2.0 * slat;
	p_p(1) = 2.0 * clat;
	p_p(2) = 4.5 * slat * slat - 1.5;
	p_p(3) = 5.1961524 * clat * slat;
	q_p(0) = -clat;
	q_p(1) = slat;
	q_p(2) = -3.0 * clat * slat;
	q_p(3) = 1.7320508 * (slat * slat - clat * clat);
	
	for (Int k=0; k<PQ_LEN; k++) {
	  if (n-m-1 < 0) {
	    m = -1;
	    n++;
	    rr = pow(ratio, Double(n+2));
	    fn = n;
	  }
	  fm = m+1;
	  if (k-4 >=0) {
	    if (m+1-n == 0) {
	      one = sqrt(1.0 - 0.5/fm);
	      j = k - n - 1;
	      p_p(k) = (1.0 + 1.0/fm) * one * clat * p_p(j);
	      q_p(k) = one * (clat * q_p(j) + slat/fm * p_p(j));
	      sl_p(m) = sl_p(m-1) * cl_p(0) + cl_p(m-1) * sl_p(0);
	      cl_p(m) = cl_p(m-1) * cl_p(0)-sl_p(m-1) * sl_p(0);
	    } else {
	      one = sqrt(Double(fn * fn - fm * fm));
	      two = sqrt((fn-1.0) * (fn-1.0) - fm * fm)/one;
	      three = (2.0 * fn - 1.0)/one;
	      i = k-n;
	      j = k - 2 * n + 1;
	      p_p(k) = (fn+1.0) * (three * slat/fn * p_p(i) -
				   two/(fn-1.0) * p_p(j));
	      q_p(k) = three * (slat * q_p(i) - clat/fn * p_p(i)) -
		two * q_p(j);
	    }
	  }
	  //
	  // Synthesise X,Y,Z in geocentric coordinates
	  //
	  one = (agh_p(l)) * rr;
	  if (m == -1) {
	    x = x + one * q_p(k);
	    z = z - one * p_p(k);
	    l++;
	  } else {
	    two = (agh_p(l+1)) * rr;
	    three = one * cl_p(m) + two * sl_p(m);
	    x = x + three * q_p(k);
	    z = z - three * p_p(k);
	    if (clat > 0) {
	      y = y + (one * sl_p(m) -
		       two * cl_p(m)) * fm * p_p(k)/((fn+1.0) * clat);
	    } else {
	      y = y + (one * sl_p(m) -
		       two * cl_p(m)) * q_p(k) * slat;
	    }
	    l += 2;
	  }
	  m++;
	} // calculation loop
	// Rotate from local vertical/meridian to ITRF one
	if (lp == 0) {
	  pval_p[0] = +x*slat*clong + z*clat*clong + y*slong;
	  pval_p[1] = -x*slat*slong + z*clat*slong - y*clong;
	  pval_p[2] = -x*clat + z*slat;
	} else {
	  dval_p[lp-1][0] = (+x*slat*clong + z*clat*clong + y*slong -
			     pval_p[0])/DER_INTV;
	  dval_p[lp-1][1] = (-x*slat*slong + z*clat*slong - y*clong -
			     pval_p[1])/DER_INTV;
	  dval_p[lp-1][2] = (-x*clat + z*slat -
			     pval_p[2])/DER_INTV;
	}
	if (lp < 3) {
	  if (lp != 0) posmv(lp-1) -= DER_INTV;
	  posmv(lp) += DER_INTV;
	  posv = MVPosition(posmv).get();
	}
      } // derivative loop
    }
    break;
    }
  }
}

} //# NAMESPACE CASACORE - END

