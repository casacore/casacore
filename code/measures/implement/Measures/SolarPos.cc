//# SolarPos.cc: Solar position class
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
typedef Quantum<Double> gpp_solarpos_bug1;
#endif
#include <aips/Measures/SolarPos.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Tasking/AipsrcValue.h>

//# Constants
const Double SolarPos::INTV = 0.04;

//# Static data
uInt SolarPos::interval_reg = 0;
uInt SolarPos::usejpl_reg = 0;

//# Constructors
SolarPos::SolarPos() : method(SolarPos::STANDARD), lres(0) {
    fill();
}

SolarPos::SolarPos(const SolarPos &other) {
    copy(other);
}

SolarPos::SolarPos(SolarPosTypes type) :
method(type), lres(0) {
    fill();
}

SolarPos &SolarPos::operator=(const SolarPos &other) {
    if (this != &other) {
	copy(other);
    }
    return *this;
}

void SolarPos::copy(const SolarPos &other) {
    method = other.method;
    checkEpoch = other.checkEpoch;
    checkSunEpoch = other.checkSunEpoch;
    for (Int i=0; i<3; i++) {
	sval[i] = other.sval[i];
	eval[i] = other.eval[i];
	dsval[i] = other.dsval[i];
	deval[i] = other.deval[i];
    };
    for (Int j=0; j<6; j++) {
	result[j] = other.result[j];
    };
}

//# Destructor
SolarPos::~SolarPos() {}

//# Operators
// Calculate Solar Position 
const MVPosition &SolarPos::operator()(Double epoch) {
    calcEarth(epoch);
    Double dt = epoch - checkEpoch;
    lres++; lres %= 6;
    for (Int i=0; i<3; i++) {
	result[lres](i) = (-eval[i] - dt*deval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

//# Member functions
const MVPosition &SolarPos::baryEarth(Double epoch) {
    calcEarth(epoch);
    calcSun(epoch);
    Int i;
    Double dt = epoch - checkEpoch;
    lres++; lres %= 6;
    for (i=0; i<3; i++) {
	result[lres](i) = eval[i] + dt*deval[i];
    };
    dt = epoch - checkSunEpoch;
    for (i=0; i<3; i++) {
	result[lres](i) -= (sval[i] + dt*dsval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

const MVPosition &SolarPos::barySun(Double epoch) {
    calcSun(epoch);
    Double dt = epoch - checkSunEpoch;
    lres++; lres %= 6;
    for (Int i=0; i<3; i++) {
	result[lres](i) = (-sval[i] - dt*dsval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

const MVPosition &SolarPos::derivative(Double epoch) {
    calcEarth(epoch);
    lres++; lres %= 6;
    for (Int i=0; i<3; i++) {
	result[lres](i) = (-deval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

const MVPosition &SolarPos::baryEarthDerivative(Double epoch) {
    calcEarth(epoch);
    calcSun(epoch);
    lres++; lres %= 6;
    for (Int i=0; i<3; i++) {
	result[lres](i) = (deval[i] - dsval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

const MVPosition &SolarPos::barySunDerivative(Double epoch) {
    calcSun(epoch);
    lres++; lres %= 6;
    for (Int i=0; i<3; i++) {
	result[lres](i) = (-dsval[i]);
    };
    // Convert to rectangular
    if (!AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
      result[lres] = MeasTable::posToRect() * result[lres];
    };
    return result[lres];
}

void SolarPos::fill() {
  // Get the interpolation interval
  if (!SolarPos::interval_reg) {
    interval_reg = 
      AipsrcValue<Double>::registerRC(String("measures.solarpos.d_interval"),
				      Unit("d"), Unit("d"),
				      SolarPos::INTV);
  };
  if (!SolarPos::usejpl_reg) {
    usejpl_reg =
      AipsrcValue<Bool>::registerRC(String("measures.solarpos.b_usejpl"),
				    False);
  };
  checkEpoch = 1e30;
  checkSunEpoch = 1e30;
}

void SolarPos::refresh() {
    checkEpoch = 1e30;
    checkSunEpoch = 1e30;
}

void SolarPos::calcEarth(Double t) {
    if (!nearAbs(t, checkEpoch,
		 AipsrcValue<Double>::get(SolarPos::interval_reg))) {
	checkEpoch = t;
	switch (method) {
	    default:
	    t = (t - MeasData::MJD2000)/MeasData::JDCEN;
	    break;
	}
	Int i,j;
	Vector<Double> fa(12), dfa(12);
	for (i=0; i<3; i++) {
	    eval[i] = deval[i] = Double(0);
	};
	Double dtmp, ddtmp;
	switch (method) {
	    default:
	      if (AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
		const Vector<Double> &mypl =
		  MeasTable::Planetary(MeasTable::EARTH, checkEpoch);
		for (i=0; i<3; i++) {
		  eval[i] = mypl(i);
		  deval[i] = mypl(i+3);
		};
                const Vector<Double> &mypl1 =
		  MeasTable::Planetary(MeasTable::SUN, checkEpoch);
		for (i=0; i<3; i++) {
		  eval[i] -= mypl1(i);
		  deval[i] -= mypl1(i+3);
		};
	      } else {
		for (i=0; i<12; i++) {
		  fa(i) = MeasTable::posArg(i)(t);
		  dfa(i) = (MeasTable::posArg(i).derivative())(t);
		}
		for (i=0; i<189; i++) {
		  dtmp = ddtmp = 0;
		  for (j=0; j<12; j++) {
		    dtmp += MeasTable::mulPosEarthXYArg(i)(j) * fa(j);
		    ddtmp += MeasTable::mulPosEarthXYArg(i)(j) * dfa(j);
		  }
		  eval[0] += MeasTable::mulPosEarthXY(i,t)(1) * 
		    sin(dtmp + MeasTable::mulPosEarthXY(i,t)(0));
		  eval[1] += MeasTable::mulPosEarthXY(i,t)(3) * 
		    sin(dtmp + MeasTable::mulPosEarthXY(i,t)(2));
		  deval[0] += MeasTable::mulPosEarthXY(i,t)(5) * 
		    sin(dtmp + MeasTable::mulPosEarthXY(i,t)(0)) +
		    MeasTable::mulPosEarthXY(i,t)(1) *
		    cos(dtmp + MeasTable::mulPosEarthXY(i,t)(0)) *
		    ddtmp;
		  deval[1] += MeasTable::mulPosEarthXY(i,t)(7) * 
		    sin(dtmp + MeasTable::mulPosEarthXY(i,t)(2)) +
		    MeasTable::mulPosEarthXY(i,t)(3) *
		    cos(dtmp + MeasTable::mulPosEarthXY(i,t)(2)) *
		    (ddtmp);
		}
		for (i=0; i<32; i++) {
		  dtmp = ddtmp = 0;
		  for (j=0; j<12; j++) {
		    dtmp += MeasTable::mulPosEarthZArg(i)(j) * fa(j);
		    ddtmp += MeasTable::mulPosEarthZArg(i)(j) * dfa(j);
		  }
		  eval[2] += MeasTable::mulPosEarthZ(i,t)(1) * 
		    sin(dtmp + MeasTable::mulPosEarthZ(i,t)(0));
		  deval[2] += MeasTable::mulPosEarthZ(i,t)(3) * 
		    sin(dtmp + MeasTable::mulPosEarthZ(i,t)(0)) +
		    MeasTable::mulPosEarthZ(i,t)(1) * 
		    cos(dtmp + MeasTable::mulPosEarthZ(i,t)(0)) *
		    (ddtmp);
		};
		for (i=0; i<3; i++) {
		  deval[i] /= MeasData::JDCEN;
		};
	      };
	      break;
	}
    };
}
    
void SolarPos::calcSun(Double t) {
    if (!nearAbs(t, checkSunEpoch,
		 AipsrcValue<Double>::get(SolarPos::interval_reg))) {
	checkSunEpoch = t;
	switch (method) {
	    default:
	    t = (t - MeasData::MJD2000)/MeasData::JDCEN;
	    break;
	}
	Int i,j;
	Vector<Double> fa(12), dfa(12);
	for (i=0; i<3; i++) {
	    sval[i] = dsval[i] = Double(0);
	};
	Double dtmp, ddtmp;
	switch (method) {
	    default:
              if (AipsrcValue<Bool>::get(SolarPos::usejpl_reg)) {
                const Vector<Double> &mypl =
                  MeasTable::Planetary(MeasTable::SUN, checkEpoch);
                for (i=0; i<3; i++) {
                  sval[i] = -mypl(i);
                  dsval[i] = -mypl(i+3);
                };
              } else {
		for (i=0; i<12; i++) {
		  fa(i) = MeasTable::posArg(i)(t);
		  dfa(i) = (MeasTable::posArg(i).derivative())(t);
		};
		for (i=0; i<98; i++) {
		  dtmp = ddtmp = 0;
		  for (j=0; j<12; j++) {
		    dtmp += MeasTable::mulPosSunXYArg(i)(j) * fa(j);
		    ddtmp += MeasTable::mulPosSunXYArg(i)(j) * dfa(j);
		  };
		  sval[0]+= MeasTable::mulPosSunXY(i,t)(1) * 
		    sin(dtmp + MeasTable::mulPosSunXY(i,t)(0));
		  sval[1] += MeasTable::mulPosSunXY(i,t)(3) * 
		    sin(dtmp + MeasTable::mulPosSunXY(i,t)(2));
		  dsval[0]+= MeasTable::mulPosSunXY(i,t)(5) * 
		    sin(dtmp + MeasTable::mulPosSunXY(i,t)(0)) +
		    MeasTable::mulPosSunXY(i,t)(1) * 
		    cos(dtmp + MeasTable::mulPosSunXY(i,t)(0)) *
		    (ddtmp);
		  dsval[1]+= MeasTable::mulPosSunXY(i,t)(7) * 
		    sin(dtmp + MeasTable::mulPosSunXY(i,t)(2)) +
		    MeasTable::mulPosSunXY(i,t)(3) * 
		    cos(dtmp + MeasTable::mulPosSunXY(i,t)(2)) *
		    (ddtmp);
		};
		for (i=0; i<29; i++) {
		  dtmp = ddtmp = 0;
		  for (j=0; j<12; j++) {
		    dtmp += MeasTable::mulPosSunZArg(i)(j) * fa(j);
		    ddtmp += MeasTable::mulPosSunZArg(i)(j) * dfa(j);
		  };
		  sval[2] += MeasTable::mulPosSunZ(i,t)(1) * 
		    sin(dtmp + MeasTable::mulPosSunZ(i,t)(0));
		  dsval[2] += MeasTable::mulPosSunZ(i,t)(3) * 
		    sin(dtmp + MeasTable::mulPosSunZ(i,t)(0)) +
		    MeasTable::mulPosSunZ(i,t)(1) * 
		    cos(dtmp + MeasTable::mulPosSunZ(i,t)(0)) *
		    (ddtmp);
		};
		for (i=0; i<3; i++) {
		  dsval[i] /= MeasData::JDCEN;
		};
	      };
	      break;
	}
    }
}
