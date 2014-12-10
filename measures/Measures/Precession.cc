//# Precession.cc:  Precession class
//# Copyright (C) 1995,1996,1997,1998,1999,2002,2003
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
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/Precession.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double Precession::INTV = 0.1;

//# Static data
uInt Precession::myInterval_reg = 0;

//# Constructors
Precession::Precession() :
method_p(Precession::STANDARD), fixedEpoch_p(MeasData::MJD2000), lres_p(0) {
  fillEpoch();
}

Precession::Precession(const Precession &other) {
  copy(other);
}

Precession::Precession(PrecessionTypes type, Double catepoch) :
method_p(type), fixedEpoch_p(catepoch), lres_p(0) {
  fillEpoch();
}

Precession &Precession::operator=(const Precession &other) {
  if ( this != &other) {
    copy(other);
  }
  return *this;
}

void Precession::init() {
  method_p = Precession::STANDARD;
  fixedEpoch_p = MeasData::MJD2000;
  fillEpoch();
}

void Precession::init(PrecessionTypes type, Double catepoch) {
  method_p = type;
  fixedEpoch_p = catepoch;
  fillEpoch();
}

//# Destructor
Precession::~Precession() {}

//# Operators
// Calculate precession Euler angles
const Euler &Precession::operator()(Double epoch) {
  calcPrec(epoch);
  Double dt = epoch - checkEpoch_p;
  lres_p++; lres_p %= 4;
  for (uInt i=0; i<3; ++i) {
    result_p[lres_p](i) = pval_p[i] + dt*dval_p[i];
  }
  return result_p[lres_p];
}

//# Member functions
// Calculate derivative of precession Euler angles
const Euler &Precession::derivative(Double epoch) {
  calcPrec(epoch);
  ++lres_p; lres_p %= 4;
  for (uInt i=0; i<3; ++i) result_p[lres_p](i) = dval_p[i];
  return result_p[lres_p];
}

void Precession::copy(const Precession &other) {
  method_p = other.method_p;
  fixedEpoch_p = other.fixedEpoch_p;
  T_p = other.T_p;
  cent_p = other.cent_p;
  refEpoch_p = other.refEpoch_p;
  checkEpoch_p = other.checkEpoch_p;
  for (uInt i=0; i<3; ++i) {
    zeta_p[i] = other.zeta_p[i];
    pval_p[i] = other.pval_p[i];
    dval_p[i] = other.dval_p[i];
  }
  for (uInt i=0; i<4; ++i) result_p[i] = other.result_p[i];
}

void Precession::fillEpoch() {
  // Get the interpolation interval
  if (!Precession::myInterval_reg) {
    myInterval_reg = 
      AipsrcValue<Double>::registerRC(String("measures.precession.d_interval"),
				      Unit("d"), Unit("d"),
				      Precession::INTV);
  }
  
  checkEpoch_p = 1e30;
  switch (method_p) {
  case B1950: 
    refEpoch_p = MeasData::MJDB1850;
    cent_p = MeasData::TROPCEN;
    break;
  default: 
    refEpoch_p = MeasData::MJD2000;
    cent_p = MeasData::JDCEN;
    break;
  }
  if (fixedEpoch_p == 0) {
    switch (method_p) {
    case B1950:
      fixedEpoch_p = MeasData::MJDB1950;
      break;
    default:
      fixedEpoch_p = refEpoch_p;
      break;
    }
  }
  switch (method_p) {
  case IAU2000:
    for (uInt i=0; i<3; ++i) zeta_p[i] = Polynomial<Double>(5);
    break;
  default:
    for (uInt i=0; i<3; ++i) zeta_p[i] = Polynomial<Double>(3);
    break;
  }
  T_p = (fixedEpoch_p - refEpoch_p)/cent_p;
  switch (method_p) {
  case B1950:
    MeasTable::precessionCoef1950(T_p, zeta_p);
  case NONE:
    break;
  case IAU2000:
    MeasTable::precessionCoef2000(zeta_p);
    break;
  default:
    MeasTable::precessionCoef(T_p, zeta_p);
    break;
  }
  for (uInt i=0; i<4; ++i) result_p[i].set(3,2,3);
}

void Precession::refresh() {
  checkEpoch_p = 1e30;
}

void Precession::calcPrec(Double t) {
  if (!nearAbs(t, checkEpoch_p,
	       AipsrcValue<Double>::get(Precession::myInterval_reg))) {
    checkEpoch_p = t;
    switch (method_p) {
    case B1950:
      t = (t - refEpoch_p)/cent_p - T_p;
      break;
    default:
      t = (t - fixedEpoch_p)/cent_p;
      break;
    }
    for (uInt i=0; i<3; ++i) {
      pval_p[i] = (zeta_p[i])(t);
      dval_p[i] = ((zeta_p[i]).derivative())(t);
      switch (method_p) {
      case B1950:
	dval_p[i] = dval_p[i]/MeasData::TROPCEN;
	break;
      default:
	dval_p[i] = dval_p[i]/MeasData::JDCEN;
	break;
      }
    }
  }
}

} //# NAMESPACE CASACORE - END

