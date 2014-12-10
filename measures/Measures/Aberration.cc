//# Aberration.cc:  Aberration class
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2002
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
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double Aberration::INTV = 0.04;

//# Static data
uInt Aberration::interval_reg = 0;
uInt Aberration::usejpl_reg = 0;

//# Constructors
Aberration::Aberration() : method(Aberration::STANDARD), lres(0) {
    fill();
}

Aberration::Aberration(const Aberration &other) {
    copy(other);
}

Aberration::Aberration(AberrationTypes type) :
method(type), lres(0) {
    fill();
}

Aberration &Aberration::operator=(const Aberration &other) {
    if (this != &other) {
	copy(other);
    }
    return *this;
}


void Aberration::init() {
    method = Aberration::STANDARD;
    fill();
}

void Aberration::init(AberrationTypes type) {
    method = type;
    fill();
}

void Aberration::copy(const Aberration &other) {
    method = other.method;
    checkEpoch = other.checkEpoch;
    for (Int i=0; i<3; i++) {
	aval[i] = other.aval[i];
	dval[i] = other.dval[i];
    }
    for (Int j=0; j<4; j++) {
	result[j] = other.result[j];
    }
}

//# Destructor
Aberration::~Aberration() {}

//# Operators
// Calculate Aberration 
const MVPosition &Aberration::operator()(Double epoch) {
    calcAber(epoch);
    Double dt = epoch - checkEpoch;
    Double fac = 1;
    if (AipsrcValue<Bool>::get(Aberration::usejpl_reg) && method != B1950) {
      fac /= MeasTable::Planetary(MeasTable::CAU);
    }
    lres++; lres %= 4;
    for (Int i=0; i<3; i++) {
      result[lres](i) = fac * (aval[i] + dt*dval[i]);
    }
    return result[lres];
}

//# Member functions

const MVPosition &Aberration::derivative(Double epoch) {
    calcAber(epoch);
    lres++; lres %= 4;
    Double fac = 1;
    if (AipsrcValue<Bool>::get(Aberration::usejpl_reg) && method != B1950) {
      fac /=  MeasTable::Planetary(MeasTable::CAU);
    }
    for (Int i=0; i<3; i++) {
      result[lres](i) = fac * dval[i];
    }
    return result[lres];
}

void Aberration::fill() {
  // Get the interpolation interval
  if (!Aberration::interval_reg) {
    interval_reg = 
      AipsrcValue<Double>::registerRC(String("measures.aberration.d_interval"),
				      Unit("d"), Unit("d"),
				      Aberration::INTV);
  }
  if (!Aberration::usejpl_reg) {
    usejpl_reg =
      AipsrcValue<Bool>::registerRC(String("measures.aberration.b_usejpl"),
				    False);
  }
  checkEpoch = 1e30;
}

void Aberration::refresh() {
    checkEpoch = 1e30;
}

void Aberration::calcAber(Double t) {
  if (!nearAbs(t, checkEpoch,
	       AipsrcValue<Double>::get(Aberration::interval_reg)) ||
      (AipsrcValue<Bool>::get(Aberration::usejpl_reg) &&
       method != B1950) ) {
    checkEpoch = t;
    switch (method) {
    case B1950:
      // Yes, this really should be the time in Julian centuries since January 	 
      // 0.5, 1900, not 1950.  And MJDB1900 should probably be named MJD1900 	 
      // since it is 15019.5, indicating a Julian instead of a Besselian (= 	 
      // tropical) date.
      t = (t - MeasData::MJDB1900)/MeasData::JDCEN;
      break;
    default:
      t = (t - MeasData::MJD2000)/MeasData::JDCEN;
      break;
    }
    Int i,j;
    Vector<Double> fa(13), dfa(13);
    for (i=0; i<3; i++) {
      aval[i] = dval[i] = Double(0);
    }
    Double dtmp, ddtmp, sdtmp, cdtmp;
    switch (method) {
    case B1950:
      {
        for (i=0; i<12; i++) {
          const Polynomial<Double>& aberArgP = MeasTable::aber1950Arg(i);
          fa(i) = aberArgP(t);
          dfa(i) = (aberArgP.derivative())(t);
        }
	CountedPtr<Matrix<Double> > mul = MeasTable::mulAber1950(t, 1e-6);
        DebugAssert (mul->contiguousStorage(), AipsError);
        const Double* mulAberV = mul->data();

        for (i=0; i<132; i++) {
          const Double* mulAberArgV = MeasTable::mulAber1950Arg(i);
          dtmp = ddtmp = 0; 
          for (j=0; j<12; j++) {
            dtmp += mulAberArgV[j] * fa(j);
            ddtmp += mulAberArgV[j] * dfa(j);
          }
          sdtmp = sin(dtmp);
          cdtmp = cos(dtmp);

          aval[0] += mulAberV[0] * sdtmp + mulAberV[1] * cdtmp;
          aval[1] += mulAberV[2] * sdtmp + mulAberV[3] * cdtmp;
          aval[2] += mulAberV[4] * sdtmp + mulAberV[5] * cdtmp;
          dval[0] += mulAberV[6] * sdtmp + mulAberV[7] * cdtmp +
            (mulAberV[0] * cdtmp - mulAberV[1] * sdtmp) * ddtmp;
          dval[1] += mulAberV[8] * sdtmp + mulAberV[9] * cdtmp +
            (mulAberV[2] * cdtmp - mulAberV[3] * sdtmp) * ddtmp;
          dval[2] += mulAberV[10] * sdtmp + mulAberV[11] * cdtmp +
            (mulAberV[4] * cdtmp - mulAberV[5] * sdtmp) * ddtmp;
          mulAberV += 12;
        }
        for (i=0; i<3; i++) {
          aval[i] /= C::c;
          dval[i] /= (C::c * MeasData::JDCEN);
        }
      }
      break;
      
    default:
      if (AipsrcValue<Bool>::get(Aberration::usejpl_reg)) {
	Vector<Double> mypl =
	  MeasTable::Planetary(MeasTable::EARTH, checkEpoch);
	for (i=0; i<3; i++) {
	  aval[i] = mypl[i + 3];
	  dval[i] = 0;
	}
      } else {
	for (i=0; i<13; i++) {
	  const Polynomial<Double>& aberArgP = MeasTable::aberArg(i);

	  fa(i) = aberArgP(t);
	  dfa(i) = (aberArgP.derivative())(t);
	}
	CountedPtr<Matrix<Double> > mul = MeasTable::mulAber(t, 1e-6);
        DebugAssert (mul->contiguousStorage(), AipsError);
        const Double* mulAberV = mul->data();
	for (i=0; i<80; i++) {
	  const Double* mulAberArgV = MeasTable::mulAberArg(i);
	  dtmp = ddtmp = 0; 
	  for (j=0; j<6; j++) {
	    dtmp  += mulAberArgV[j] * fa[j];
	    ddtmp += mulAberArgV[j] * dfa[j];
	  }
	  sdtmp = sin(dtmp);
	  cdtmp = cos(dtmp);
	  
	  aval[0] += mulAberV[0] * sdtmp + mulAberV[1] * cdtmp;
	  aval[1] += mulAberV[2] * sdtmp + mulAberV[3] * cdtmp;
	  aval[2] += mulAberV[4] * sdtmp + mulAberV[5] * cdtmp;
	  dval[0] += mulAberV[6] * sdtmp + mulAberV[7] * cdtmp +
	    (mulAberV[0] * cdtmp - mulAberV[1] * sdtmp) * ddtmp;
	  dval[1] += mulAberV[8] * sdtmp + mulAberV[9] * cdtmp +
	    (mulAberV[2] * cdtmp - mulAberV[3] * sdtmp) * ddtmp;
	  dval[2] += mulAberV[10] * sdtmp + mulAberV[11] * cdtmp +
	    (mulAberV[4] * cdtmp - mulAberV[5] * sdtmp) * ddtmp;
          mulAberV += 12;
	}
	for (i=0; i<17; i++) {
	  const Double* mulAberArgV = MeasTable::mulAberSunArg(i);

	  dtmp = ddtmp = 0;
	  for (j=0; j<7; j++) {
	    dtmp  += mulAberArgV[j] * fa[j + 1];
	    ddtmp += mulAberArgV[j] * dfa[j + 1];
	  }
	  
	  sdtmp = sin(dtmp);
	  cdtmp = cos(dtmp);

	  const Vector<Double>& mulAberV = MeasTable::mulSunAber(i);

	  aval[0] += mulAberV[0] * sdtmp + mulAberV[1] * cdtmp;
	  aval[1] += mulAberV[2] * sdtmp + mulAberV[3] * cdtmp;
	  aval[2] += mulAberV[4] * sdtmp + mulAberV[5] * cdtmp;
	  dval[0] += (mulAberV[0] * cdtmp - mulAberV[1] * sdtmp) * ddtmp;
	  dval[1] += (mulAberV[2] * cdtmp - mulAberV[3] * sdtmp) * ddtmp;
	  dval[2] += (mulAberV[4] * cdtmp - mulAberV[5] * sdtmp) * ddtmp;
	}
	for (i=0; i<17; i++) {
	  const Double* mulAberArgV = MeasTable::mulAberEarthArg(i);

	  dtmp = ddtmp = 0;
	  for (j=0; j<5; j++) {
	    dtmp  += mulAberArgV[j] *  fa[j + 8];
	    ddtmp += mulAberArgV[j] * dfa[j + 8];
	  }

	  sdtmp = sin(dtmp);
	  cdtmp = cos(dtmp);
	          
	  const Vector<Double>& mulAberV = MeasTable::mulEarthAber(i);

	  aval[0] += mulAberV[0] * sdtmp;
	  aval[1] += mulAberV[1] * cdtmp;
	  aval[2] += mulAberV[2] * cdtmp;
	  dval[0] += mulAberV[0] * cdtmp * ddtmp;
	  dval[1] += -mulAberV[1] * sdtmp * ddtmp;
	  dval[2] += -mulAberV[2] * sdtmp * ddtmp;
	}
	for (i=0; i<3; i++) {
	  aval[i] /= C::c;
	  dval[i] /= (C::c * MeasData::JDCEN);
	}
      }
      break;
    }
  }
}

} //# NAMESPACE CASACORE - END

