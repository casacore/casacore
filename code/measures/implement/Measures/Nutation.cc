//# Nutation.cc:  Nutation class
//# Copyright (C) 1995,1996,1997,1998
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
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_nutation_bug1;
#endif
#include <aips/Measures/Nutation.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Functionals/Polynomial.h>
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
method(Nutation::STANDARD), lres(0) {
    fill();
}

Nutation::Nutation(const Nutation &other) {
    copy(other);
}

Nutation::Nutation(NutationTypes type) :
method(type), lres(0) {
    fill();
}

Nutation &Nutation::operator=(const Nutation &other) {
    if (this != &other) {
	copy(other);
    }
    return *this;
}

void Nutation::init() {
    method = Nutation::STANDARD;
    fill();
}

void Nutation::init(NutationTypes type) {
    method = type;
    fill();
}

void Nutation::copy(const Nutation &other) {
    method = other.method;
    checkEpoch = other.checkEpoch;
    eqeq = other.eqeq;
    deqeq = other.deqeq;
    for (Int i=0; i<3; i++) {
	nval[i] = other.nval[i];
	dval[i] = other.dval[i];
    };
    for (Int j=0; j<4; j++) {
	result[j] = other.result[j];
    };
}

//# Destructor
Nutation::~Nutation() {}

//# Operators
// Calculate Nutation Euler angles
const Euler &Nutation::operator()(Double epoch) {
    calcNut(epoch);
    lres++; lres %= 4;
    Double dt = epoch - checkEpoch;
    for (Int i=0; i<3; i++) {
	result[lres](i) = nval[i] + dt*dval[i];
    };
    return result[lres];
}

//# Member functions

const Euler &Nutation::derivative(Double epoch) {
    calcNut(epoch);
    lres++; lres %= 4;
    for (Int i=0; i<3; i++) {
	result[lres](i) = dval[i];
    };
    return result[lres];
}

void Nutation::fill() {
    checkEpoch = 1e30;
    for (Int i=0; i<4; i++) {
	result[i].set(1,3,1);
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
    checkEpoch = 1e30;
}

Double Nutation::eqox(Double epoch) {
    calcNut(epoch);
    return (Double(eqeq + (epoch - checkEpoch)*deqeq));
}

Double Nutation::derivativeEqox(Double epoch) {
    calcNut(epoch);
    return deqeq;
}

Quantity Nutation::getEqoxAngle(Double epoch) {
    return Quantity(eqox(epoch),"rad");
}

Quantity Nutation::getEqoxAngle(Double epoch, const Unit &unit) {
    return Quantity(eqox(epoch),"rad").get(unit);
}

void Nutation::calcNut(Double t) {
  if (!nearAbs(t, checkEpoch,
	       AipsrcValue<Double>::get(Nutation::interval_reg))) {
    checkEpoch = t;
    Double dEps = 0;
    Double dPsi = 0;
    switch (method) {
    case B1950:
      t = (t - MeasData::MJDB1900)/MeasData::JDCEN;
      break;
    default:
      if (AipsrcValue<Bool>::get(Nutation::useiers_reg)) {
	dPsi = MeasTable::dPsiEps(0, t);
	dEps = MeasTable::dPsiEps(1, t);
      };
      t = (t - MeasData::MJD2000)/MeasData::JDCEN;
      break;
      };
    Int i,j;
    Vector<Double> fa(5), dfa(5);
    Double dtmp, ddtmp;
    nval[1] = Double(0);
    dval[1] = Double(0);
    nval[2] = Double(0);
    dval[2] = Double(0);
    switch (method) {
    case B1950:
      nval[0] = MeasTable::fundArg1950(0)(t); 	//eps0
      dval[0] = (MeasTable::fundArg1950(0).derivative())(t);
      for (i=0; i<5; i++) {
	fa(i) = MeasTable::fundArg1950(i+1)(t);
	dfa(i) = (MeasTable::fundArg1950(i+1).derivative())(t);
      }
      for (i=0; i<69; i++) {
	dtmp = ddtmp = 0;
	for (j=0; j<5; j++) {
	  dtmp += MeasTable::mulArg1950(i)(j) * fa(j);
	  ddtmp += MeasTable::mulArg1950(i)(j) * dfa(j);
	}
	nval[1] += MeasTable::mulSC1950(i,t)(0) * sin(dtmp);
	nval[2] += MeasTable::mulSC1950(i,t)(1) * cos(dtmp);
	dval[1] += MeasTable::mulSC1950(i,t)(2) * sin(dtmp) +
	  MeasTable::mulSC1950(i,t)(0) * cos(dtmp) * ddtmp;
	dval[2] += MeasTable::mulSC1950(i,t)(3) * cos(dtmp) -
	  MeasTable::mulSC1950(i,t)(1) * sin(dtmp) * ddtmp;
      }
      break;
    default:
      nval[0] = MeasTable::fundArg(0)(t); 	//eps0
      dval[0] = (MeasTable::fundArg(0).derivative())(t)/MeasData::JDCEN;
      if (AipsrcValue<Bool>::get(Nutation::usejpl_reg)) {
	const Vector<Double> &mypl =
	  MeasTable::Planetary(MeasTable::NUTATION, checkEpoch);
	nval[1] = mypl(0);
	nval[2] = mypl(1);
	dval[1] = mypl(2)*MeasData::JDCEN;
	dval[2] = mypl(3)*MeasData::JDCEN;
      } else {
	for (i=0; i<5; i++) {
	  fa(i) = MeasTable::fundArg(i+1)(t);
	  dfa(i) = (MeasTable::fundArg(i+1).derivative())(t);
	}
	for (i=0; i<106; i++) {
	  dtmp = ddtmp = 0;
	  for (j=0; j<5; j++) {
	    dtmp += MeasTable::mulArg(i)(j) * fa(j);
	    ddtmp += MeasTable::mulArg(i)(j) * dfa(j);
	  }
	  nval[1] += MeasTable::mulSC(i,t)(0) * sin(dtmp);
	  nval[2] += MeasTable::mulSC(i,t)(1) * cos(dtmp);
	  dval[1] += MeasTable::mulSC(i,t)(2) * sin(dtmp) +
	    MeasTable::mulSC(i,t)(0) * cos(dtmp) * ddtmp;
	  dval[2] += MeasTable::mulSC(i,t)(3) * cos(dtmp) -
	    MeasTable::mulSC(i,t)(1) * sin(dtmp) * ddtmp;
	};
      };
      nval[2] += dEps;
      nval[1] += dPsi;
      break;
    }
    nval[1] = -nval[1];
    dval[1] = -dval[1]/MeasData::JDCEN;
    nval[2] = -nval[0] - nval[2];
    dval[2] = (-dval[0] - dval[2])/MeasData::JDCEN;
    eqeq = -nval[1] * cos(nval[2]);
    deqeq = -dval[1] * cos(nval[2]) + nval[1] * sin(nval[2]) * dval[2];
  }
}

