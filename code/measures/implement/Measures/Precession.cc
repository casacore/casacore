//# Precession.cc:  Precession class
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
typedef Quantum<Double> gpp_mvdoppler_bug1;
#endif
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/Precession.h>
#include <aips/Mathematics/Math.h>
#include <aips/Tasking/AipsrcValue.h>

//# Constants
const Double Precession::INTV = 0.1;

//# Static data
uInt Precession::interval_reg = 0;

//# Constructors
Precession::Precession() :
method(Precession::STANDARD), lres(0), fixedEpoch(MeasData::MJD2000) {
    fillEpoch();
}

Precession::Precession(const Precession &other) {
    copy(other);
}

Precession::Precession(PrecessionTypes type, Double catepoch) :
method(type), lres(0), fixedEpoch(catepoch) {
    fillEpoch();
}

Precession &Precession::operator=(const Precession &other) {
    if ( this != &other) {
	copy(other);
    }
    return *this;
}

void Precession::init() {
    method = Precession::STANDARD;
    fixedEpoch = MeasData::MJD2000;
    fillEpoch();
}

void Precession::init(PrecessionTypes type, Double catepoch) {
    method = type;
    fixedEpoch = catepoch;
    fillEpoch();
}

//# Destructor
Precession::~Precession() {}

//# Operators
// Calculate precession Euler angles
const Euler &Precession::operator()(Double epoch) {
    calcPrec(epoch);
    Double dt = epoch - checkEpoch;
    lres++; lres %= 4;
    for (Int i=0; i<3; i++) {
	result[lres](i) = pval[i] + dt*dval[i];
    };
    return result[lres];
}

//# Member functions
// Calculate derivative of precession Euler angles
const Euler &Precession::derivative(Double epoch) {
    calcPrec(epoch);
    lres++; lres %= 4;
    for (Int i=0; i<3; i++) {
	result[lres](i) = dval[i];
    };
    return result[lres];
}

void Precession::copy(const Precession &other) {
    method = other.method;
    fixedEpoch = other.fixedEpoch;
    T = other.T;
    cent = other.cent;
    refEpoch = other.refEpoch;
    checkEpoch = other.checkEpoch;
    for (Int i=0; i<3; i++) {
	zeta[i] = other.zeta[i];
	pval[i] = other.pval[i];
	dval[i] = other.dval[i];
    }
    for (Int j=0; j<4; j++) {
	result[j] = other.result[j];
    };
}

void Precession::fillEpoch() {
  // Get the interpolation interval
  if (!Precession::interval_reg) {
    interval_reg = 
      AipsrcValue<Double>::registerRC(String("measures.precession.d_interval"),
				      Unit("d"), Unit("d"),
				      Precession::INTV);
  };
				      
    checkEpoch = 1e30;
    switch (method) {
	case B1950: 
	refEpoch = MeasData::MJDB1850;
	cent = MeasData::TROPCEN;
	break;
	default: 
	refEpoch = MeasData::MJD2000;
	cent = MeasData::JDCEN;
	break;
    };
    if (fixedEpoch == 0) {
	switch (method) {
	    case B1950:
	    fixedEpoch = MeasData::MJDB1950;
	    break;
	    default:
	    fixedEpoch = refEpoch;
	    break;
	};
    };
    for (Int i=0; i<3; i++) {
	zeta[i] = Polynomial<Double>(3);
    }
    T = (fixedEpoch - refEpoch)/cent;
    switch (method) {
	case B1950:
	MeasTable::precessionCoef1950(T, zeta);
	case NONE:
	break;
	default:
	MeasTable::precessionCoef(T, zeta);
	break;
    }
    for (Int j=0; j<4; j++) {
	result[j].set(3,2,3);
    };
}

void Precession::refresh() {
    checkEpoch = 1e30;
}

void Precession::calcPrec(Double t) {
    Double intv;
    if (!nearAbs(t, checkEpoch,
		 AipsrcValue<Double>::get(Precession::interval_reg))) {
	checkEpoch = t;
	switch (method) {
	    case B1950:
	    t = (t - refEpoch)/cent - T;
	    break;
	    default:
	    t = (t - fixedEpoch)/cent;
	    break;
	};
	for (Int i=0; i<3; i++) {
	    pval[i] = (zeta[i])(t);
	    dval[i] = ((zeta[i]).derivative())(t);
	    switch (method) {
		case B1950:
		dval[i] = dval[i]/MeasData::TROPCEN;
		break;
		default:
		dval[i] = dval[i]/MeasData::JDCEN;
		break;
	    };
	};
    }
}
