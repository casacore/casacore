//# SpectralElement.cc: Describes (a set of related) spectral lines
//# Copyright (C) 2001,2004
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

#include <components/SpectralComponents/SpectralElement.h>

#include <casa/BasicSL/Constants.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/MUString.h>
#include <scimath/Mathematics/AutoDiffMath.h>
#include <scimath/Functionals/CompiledFunction.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

SpectralElement::SpectralElement(const SpectralElement &other)
: tp_p(other.tp_p), par_p(other.par_p.copy()), err_p(other.err_p.copy()),
  fix_p(other.fix_p.copy()) {}

SpectralElement::~SpectralElement() {}

SpectralElement &SpectralElement::operator=(
	const SpectralElement &other
) {
	if (this != &other) {
		tp_p = other.tp_p;
		par_p.resize(other.par_p.size());
		par_p = other.par_p.copy();
		err_p.resize(other.err_p.size());
		err_p = other.err_p.copy();
		fix_p.resize(other.fix_p.size());
		fix_p = other.fix_p.copy();
	}
	return *this;
}

Double SpectralElement::operator[](const uInt n) const {
	if (n >= par_p.nelements()) {
		throw(AipsError("SpectralElement: Illegal index for parameter"));
	}
	return par_p(n);
}

Bool SpectralElement::operator==(
	const SpectralElement& other
) const {
	if (this == &other) {
		return True;
	}
	return (
		tp_p == other.tp_p && allNear(par_p, other.par_p, 1e-8)
		&& allNear(err_p, other.err_p, 1e-8)
		&& allTrue(fix_p == other.fix_p)
	);
}

const String* SpectralElement::allTypes(
	Int &nall, const SpectralElement::Types *&typ
) {
	static const String tname[SpectralElement::N_Types] = {
		String("GAUSSIAN"),
		String("POLYNOMIAL"),
		String("COMPILED"),
		String("GAUSSIAN MULTIPLET"),
		String("LORENTZIAN")

	};

	static const SpectralElement::Types oname[SpectralElement::N_Types] = {
		SpectralElement::GAUSSIAN,
		SpectralElement::POLYNOMIAL,
		SpectralElement::COMPILED,
		SpectralElement::GMULTIPLET,
		SpectralElement::LORENTZIAN
	};

	nall = SpectralElement::N_Types;
	typ    = oname;
	return tname;
}

void SpectralElement::_set(const Vector<Double>& params) {
	par_p = params.copy();
}

void SpectralElement::_setType(const SpectralElement::Types type) {
	tp_p = type;
}

const String &SpectralElement::fromType(SpectralElement::Types tp) {
	Int nall;
	const SpectralElement::Types *typ;
	const String *const tname = SpectralElement::allTypes(nall, typ);
	return tname[tp];
}

Bool SpectralElement::toType(
	SpectralElement::Types &tp, const String &typname
) {
	Int nall;
	const SpectralElement::Types *typ;
	const String *const tname = SpectralElement::allTypes(nall, typ);

	// Make sure a value returned
	tp = typ[0];
	Int i = MUString::minimaxNC(typname, SpectralElement::N_Types, tname);
	if (i >= nall) {
		return False;
	}
	tp = typ[i];
	return True;
}

void SpectralElement::get(Vector<Double> &param) const {
  param.resize(par_p.nelements());
  param = par_p.copy();
}

Vector<Double> SpectralElement::get() const {
	return par_p.copy();
}

void SpectralElement::getError(Vector<Double> &err) const {
	err.resize(err_p.nelements());
	err = err_p.copy();
}

Vector<Double> SpectralElement::getError() const {
	return err_p.copy();
}

void SpectralElement::setError(const Vector<Double> &err) {
	if (err.nelements() != par_p.nelements()) {
		throw(
			AipsError(
				"SpectralElement: setting incorrect "
				"number of errors in the element"
			)
		);
	}
	err_p = err.copy();
}

void SpectralElement::fix(const Vector<Bool> &fix) {
	if (fix.nelements() != par_p.nelements()) {
		throw(
			AipsError(
				"SpectralElement: setting incorrect number of fixed "
				"in the element"
			)
		);
	}
	fix_p = fix.copy();
}

const Vector<Bool>& SpectralElement::fixed() const {
	return fix_p;
}

Bool SpectralElement::toRecord(RecordInterface &out) const {
	out.define(RecordFieldId("type"), fromType(tp_p));
	Vector<Double> ptmp(par_p.copy());
	Vector<Double> etmp(err_p.copy());
	out.define(RecordFieldId("parameters"), ptmp);
	out.define(RecordFieldId("errors"), etmp);
	return True;
}

void SpectralElement::set(const Vector<Double>& params) {
	if (params.size() != get().size()) {
		throw AipsError(
			"Input number of parameters does not match "
			"the current number of parameters"
		);
	}
	_set(params);
}

void SpectralElement::_construct(
	const Types type, const Vector<Double>& params
) {
	tp_p = type;
	par_p = params.copy();
	err_p = Vector<Double>(params.size(), 0);
	fix_p = Vector<Bool>(params.size(), 0);
}

} //# NAMESPACE CASA - END


