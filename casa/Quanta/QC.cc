//# QC.cc: physical constants with units
//# Copyright (C) 1994-1998,2007
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
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/UnitMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Quantum<Double> QC::c(C::c,"m/s");
Quantum<Double> QC::G(6.67259e-11,"N.m2/kg2");
Quantum<Double> QC::h(6.6260755e-34,"J.s");
Quantum<Double> QC::HI(1420.405751786, "MHz");
Quantum<Double> QC::R(8.314510,"J/K/mol");
Quantum<Double> QC::NA(6.0221367e+23,"mol-1");
Quantum<Double> QC::e(1.60217733e-19,"C");
Quantum<Double> QC::mp(1.6726231e-27,"kg");
Quantum<Double> QC::mp_me(1836.152701,"");
Quantum<Double> QC::mu0(4.0e-7*C::pi,"H/m");
Quantum<Double> QC::epsilon0(1.0/(4.0e-7*C::pi*C::c*C::c),"F/m");
Quantum<Double> QC::k(8.314510/6.0221367e+23,"J/K");
Quantum<Double> QC::F(6.0221367e+23*1.60217733e-19,"C/mol");
Quantum<Double> QC::me(1.6726231e-27/1836.152701,"kg");
Quantum<Double> QC::re(2.8179e-15,"m");
Quantum<Double> QC::a0(5.2918e-11,"m");
Quantum<Double> QC::R0(6.9599e+08,"m");
Quantum<Double> QC::k2(IAU_k*IAU_k,"AU3/d2/S0");
Quantum<Double> QC::qTurn(90.0, "deg");
Quantum<Double> QC::hTurn(180.0, "deg");
Quantum<Double> QC::fTurn(360.0, "deg");

uShort QC_init::count;

QC_init::QC_init() {
    if (count++ == 0) {
        UnitMap::clearCache();
	QC::init();		// make sure statics initialized
    }
}

QC_init::~QC_init() {
    if (--count == 0) {
      UnitMap::releaseUM();	// make sure UnitMaps released
    }
}

void QC::init() {
}

} //# NAMESPACE CASACORE - END

