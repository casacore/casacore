//# QC.cc: physical constants with units
//# Copyright (C) 1994,1995,1996,1997,1998
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
#include <aips/Quanta/QC.h>
#include <aips/Quanta/UnitMap.h>

Quantum<Double> QC::c(C::c,"m/s");
Quantum<Double> QC::G(C::Gravity,"N.m2/kg2");
Quantum<Double> QC::h(C::Planck,"J.s");
Quantum<Double> QC::HI(1420.405751786, "MHz");
Quantum<Double> QC::R(C::GasConst,"J/K/mol");
Quantum<Double> QC::NA(C::Avogadro,"mol-1");
Quantum<Double> QC::e(C::qe,"C");
Quantum<Double> QC::mp(C::mp,"kg");
Quantum<Double> QC::mp_me(C::mp_me,"");
Quantum<Double> QC::mu0(C::mu0,"H/m");
Quantum<Double> QC::epsilon0(C::epsilon0,"F/m");
Quantum<Double> QC::k(C::Boltzmann,"J/K");
Quantum<Double> QC::F(C::Faraday,"C/mol");
Quantum<Double> QC::me(C::me,"kg");
Quantum<Double> QC::re(C::re,"m");
Quantum<Double> QC::a0(C::a0,"m");
Quantum<Double> QC::R0(C::R0,"m");
Quantum<Double> QC::k2(IAU_k*IAU_k,"AU3/d2/S0");

uShort QC_init::count;

QC_init::QC_init() {
    if (count++ == 0) {
        UnitMap::clearCache();
	QC::init();		// make sure statics initialized
    }
}

QC_init::~QC_init() {
    if (--count == 0) {
    }
}

void QC::init() {
}
