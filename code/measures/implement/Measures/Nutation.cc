//# Nutation.cc:  Nutation class
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
#include <aips/Measures/Nutation.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MeasIERS.h>
#include <aips/Tasking/AipsrcValue.h>

//# Constants
const Double Nutation::INTV = 0.04;

//# Static data
uInt Nutation::interval_reg = 0;
uInt Nutation::useiers_reg = 0;
uInt Nutation::usejpl_reg = 0;

//# Constructors
Nutation::Nutation() :
method_p(Nutation::STANDARD), lres_p(0) {
  fill();
}

Nutation::Nutation(const Nutation &other) {
  copy(other);
}

Nutation::Nutation(NutationTypes type) :
method_p(type), lres_p(0) {
  fill();
}

Nutation &Nutation::operator=(const Nutation &other) {
  if (this != &other) copy(other);
  return *this;
}

void Nutation::init() {
  method_p = Nutation::STANDARD;
  fill();
}

void Nutation::init(NutationTypes type) {
  method_p = type;
  fill();
}

void Nutation::copy(const Nutation &other) {
  method_p = other.method_p;
  checkEpoch_p = other.checkEpoch_p;
  eqeq_p = other.eqeq_p;
  deqeq_p = other.deqeq_p;
  for (uInt i=0; i<3; i++) {
    nval_p[i] = other.nval_p[i];
    dval_p[i] = other.dval_p[i];
  };
  for (Int j=0; j<4; j++) {
    result_p[j] = other.result_p[j];
  };
}

//# Destructor
Nutation::~Nutation() {}

//# Operators
// Calculate Nutation Euler angles
const Euler &Nutation::operator()(Double epoch) {
  calcNut(epoch);
  lres_p++; lres_p %= 4;
  Double dt = epoch - checkEpoch_p;
  for (uInt i=0; i<3; i++) {
    result_p[lres_p](i) = nval_p[i] + dt*dval_p[i];
  };
  return result_p[lres_p];
}

//# Member functions

const Euler &Nutation::derivative(Double epoch) {
  calcNut(epoch);
  lres_p++; lres_p %= 4;
  for (uInt i=0; i<3; i++) {
    result_p[lres_p](i) = dval_p[i];
  };
  return result_p[lres_p];
}

void Nutation::fill() {
  checkEpoch_p = 1e30;
  for (uInt i=0; i<4; i++) {
    result_p[i].set(1,3,1);
  };
  // Get interval and other switches
  if (!Nutation::interval_reg) {
    interval_reg = 
      AipsrcValue<Double>::registerRC(String("measures.nutation.d_interval"),
				      Unit("d"), Unit("d"),
				      Nutation::INTV);
  };
  if (!Nutation::useiers_reg) {
    useiers_reg =
      AipsrcValue<Bool>::registerRC(String("measures.nutation.b_useiers"),
				    False);
  };
  if (!Nutation::usejpl_reg) {
    usejpl_reg =
      AipsrcValue<Bool>::registerRC(String("measures.nutation.b_usejpl"),
				    False);
  };
}

void Nutation::refresh() {
  checkEpoch_p = 1e30;
}

Double Nutation::eqox(Double epoch) {
  calcNut(epoch);
  return (Double(eqeq_p + (epoch - checkEpoch_p)*deqeq_p));
}

Double Nutation::derivativeEqox(Double epoch) {
  calcNut(epoch);
  return deqeq_p;
}

Quantity Nutation::getEqoxAngle(Double epoch) {
  return Quantity(eqox(epoch),"rad");
}

Quantity Nutation::getEqoxAngle(Double epoch, const Unit &unit) {
  return Quantity(eqox(epoch),"rad").get(unit);
}

void Nutation::calcNut(Double t) {
  if (!nearAbs(t, checkEpoch_p,
	       AipsrcValue<Double>::get(Nutation::interval_reg))) {
    checkEpoch_p = t;
    Double dEps = 0;
    Double dPsi = 0;
    switch (method_p) {
    case B1950:
      t = (t - MeasData::MJDB1900)/MeasData::JDCEN;
      break;
    case IAU1980:
      if (AipsrcValue<Bool>::get(Nutation::useiers_reg)) {
	dPsi = MeasTable::dPsiEps(0, t);
	dEps = MeasTable::dPsiEps(1, t);
      };
    default:
      t = (t - MeasData::MJD2000)/MeasData::JDCEN;
      break;
    };
    Vector<Double> fa(5), dfa(5);
    Double dtmp, ddtmp;
    nval_p[1] = Double(0);
    dval_p[1] = Double(0);
    nval_p[2] = Double(0);
    dval_p[2] = Double(0);
    switch (method_p) {
    case B1950:
      nval_p[0] = MeasTable::fundArg1950(0)(t); 	//eps0
      dval_p[0] = (MeasTable::fundArg1950(0).derivative())(t);
      for (uInt i=0; i<5; i++) {
	fa(i) = MeasTable::fundArg1950(i+1)(t);
	dfa(i) = (MeasTable::fundArg1950(i+1).derivative())(t);
      };
      for (uInt i=0; i<69; i++) {
	dtmp = ddtmp = 0;
	for (uInt j=0; j<5; j++) {
	  dtmp += MeasTable::mulArg1950(i)(j) * fa(j);
	  ddtmp += MeasTable::mulArg1950(i)(j) * dfa(j);
	};
	nval_p[1] += MeasTable::mulSC1950(i,t)(0) * sin(dtmp);
	nval_p[2] += MeasTable::mulSC1950(i,t)(1) * cos(dtmp);
	dval_p[1] += MeasTable::mulSC1950(i,t)(2) * sin(dtmp) +
	  MeasTable::mulSC1950(i,t)(0) * cos(dtmp) * ddtmp;
	dval_p[2] += MeasTable::mulSC1950(i,t)(3) * cos(dtmp) -
	  MeasTable::mulSC1950(i,t)(1) * sin(dtmp) * ddtmp;
      };
      break;
    case IAU2000B:
      nval_p[0] = MeasTable::fundArg2000(0)(t); 	//eps0
      dval_p[0] = (MeasTable::fundArg2000(0).derivative())(t)/MeasData::JDCEN;
      for (uInt i=0; i<5; i++) {
	fa(i) = MeasTable::fundArg2000(i+1)(t);
	dfa(i) = (MeasTable::fundArg2000(i+1).derivative())(t);
      };
      for (uInt i=0; i<77; i++) {
	dtmp = ddtmp = 0;
	for (uInt j=0; j<5; j++) {
	  dtmp += MeasTable::mulArg2000B(i)(j) * fa(j);
	  ddtmp += MeasTable::mulArg2000B(i)(j) * dfa(j);
	};
	nval_p[1] += MeasTable::mulSC2000B(i,t)(0) * sin(dtmp);
	nval_p[2] += MeasTable::mulSC2000B(i,t)(1) * cos(dtmp);
	dval_p[1] += MeasTable::mulSC2000B(i,t)(2) * sin(dtmp) +
	  MeasTable::mulSC2000B(i,t)(0) * cos(dtmp) * ddtmp;
	dval_p[2] += MeasTable::mulSC2000B(i,t)(3) * cos(dtmp) -
	  MeasTable::mulSC2000B(i,t)(1) * sin(dtmp) * ddtmp;
	nval_p[1] += MeasTable::mulSC2000B(i,t)(4) * cos(dtmp);
	nval_p[2] += MeasTable::mulSC2000B(i,t)(5) * sin(dtmp);
      };
      nval_p[2] += dEps;
      nval_p[1] += dPsi;
      break;
    default:
      nval_p[0] = MeasTable::fundArg(0)(t); 	//eps0
      dval_p[0] = (MeasTable::fundArg(0).derivative())(t)/MeasData::JDCEN;
      if (AipsrcValue<Bool>::get(Nutation::usejpl_reg)) {
	const Vector<Double> &mypl =
	  MeasTable::Planetary(MeasTable::NUTATION, checkEpoch_p);
	nval_p[1] = mypl(0);
	nval_p[2] = mypl(1);
	dval_p[1] = mypl(2)*MeasData::JDCEN;
	dval_p[2] = mypl(3)*MeasData::JDCEN;
      } else {
	for (uInt i=0; i<5; i++) {
	  fa(i) = MeasTable::fundArg(i+1)(t);
	  dfa(i) = (MeasTable::fundArg(i+1).derivative())(t);
	};
	for (uInt i=0; i<106; i++) {
	  dtmp = ddtmp = 0;
	  for (uInt j=0; j<5; j++) {
	    dtmp += MeasTable::mulArg(i)(j) * fa(j);
	    ddtmp += MeasTable::mulArg(i)(j) * dfa(j);
	  };
	  nval_p[1] += MeasTable::mulSC(i,t)(0) * sin(dtmp);
	  nval_p[2] += MeasTable::mulSC(i,t)(1) * cos(dtmp);
	  dval_p[1] += MeasTable::mulSC(i,t)(2) * sin(dtmp) +
	    MeasTable::mulSC(i,t)(0) * cos(dtmp) * ddtmp;
	  dval_p[2] += MeasTable::mulSC(i,t)(3) * cos(dtmp) -
	    MeasTable::mulSC(i,t)(1) * sin(dtmp) * ddtmp;
	};
      };
      nval_p[2] += dEps;
      nval_p[1] += dPsi;
      break;
    }
    nval_p[1] = -nval_p[1];
    dval_p[1] = -dval_p[1]/MeasData::JDCEN;
    nval_p[2] = -nval_p[0] - nval_p[2];
    dval_p[2] = (-dval_p[0] - dval_p[2])/MeasData::JDCEN;
    eqeq_p = -nval_p[1] * cos(nval_p[2]);
    deqeq_p = -dval_p[1] * cos(nval_p[2]) +
      nval_p[1] * sin(nval_p[2]) * dval_p[2];
  };
}

