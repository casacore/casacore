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
#include <scimath/Functionals/Function.h>

//debug only
#include <scimath/Functionals/CompiledFunction.h>
#include <casa/Arrays/ArrayIO.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

SpectralElement::SpectralElement(SpectralElement::Types type, const Vector<Double>& parms)
	: _type(type), _params(parms), _errors(parms.size(), 0),
	  _fixed(parms.size(), False) {}


SpectralElement::SpectralElement(const SpectralElement &other)
: _type(other._type), _params(other._params.copy()), _errors(other._errors.copy()),
  _fixed(other._fixed.copy()),
  _function(
		CountedPtr<Function<Double, Double> >(
			other._function->clone()
		)
	) {}

SpectralElement::~SpectralElement() {}

SpectralElement &SpectralElement::operator=(
	const SpectralElement &other
) {
	if (this != &other) {
		_type = other._type;
		uInt n = other._params.size();
		_params.resize(n);
		_params = other._params.copy();
		_errors.resize(n);
		_errors = other._errors.copy();
		_fixed.resize(n);
		_fixed = other._fixed.copy();
		_function = CountedPtr<Function<Double, Double> >(
			other._function->clone()
		);
	}
	return *this;
}

Double SpectralElement::operator[](const uInt n) const {
	if (n >= _params.size()) {
		throw(AipsError("SpectralElement: Illegal index for parameter"));
	}
	return _params[n];
}

Bool SpectralElement::operator==(
	const SpectralElement& other
) const {
	if (this == &other) {
		return True;
	}
	return (
		_type == other._type && allNear(_params, other._params, 1e-8)
		&& allNear(_errors, other._errors, 1e-8)
		&& allTrue(_fixed == other._fixed)
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
		String("LORENTZIAN"),
		String("POWER LOGARITHMIC POLYNOMIAL"),
		String("LOGARITHMIC TRANSFORMED POLYNOMIAL")

	};

	static const SpectralElement::Types oname[SpectralElement::N_Types] = {
		SpectralElement::GAUSSIAN,
		SpectralElement::POLYNOMIAL,
		SpectralElement::COMPILED,
		SpectralElement::GMULTIPLET,
		SpectralElement::LORENTZIAN,
		SpectralElement::POWERLOGPOLY,
		SpectralElement::LOGTRANSPOLY

	};

	nall = SpectralElement::N_Types;
	typ    = oname;
	return tname;
}

void SpectralElement::_set(const Vector<Double>& params) {
	_params = params.copy();
	for (uInt i=0; i<params.size(); i++) {
		(*_function)[i] = params[i];
	}
}

void SpectralElement::_setType(const SpectralElement::Types type) {
	_type = type;
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

void SpectralElement::_setFunction(
	const CountedPtr<Function<Double, Double> >& f
) {
	_function = f;
}

Double SpectralElement::operator()(const Double x) const {
	return (*_function)(x);
}

void SpectralElement::get(Vector<Double> &param) const {
	param = _params.copy();
}

Vector<Double> SpectralElement::get() const {
	return _params.copy();
}

void SpectralElement::getError(Vector<Double> &err) const {
	err = _errors.copy();
}

Vector<Double> SpectralElement::getError() const {
	return _errors.copy();
}

void SpectralElement::setError(const Vector<Double> &err) {
	if (err.nelements() != _params.nelements()) {
		throw(
			AipsError(
				"SpectralElement: setting incorrect "
				"number of errors in the element"
			)
		);
	}
	_errors = err.copy();
}

void SpectralElement::fix(const Vector<Bool> &fix) {
	if (fix.nelements() != _params.nelements()) {
		throw(
			AipsError(
				"SpectralElement: setting incorrect number of fixed "
				"in the element"
			)
		);
	}
	_fixed = fix.copy();
	for (uInt i=0; i<_fixed.size(); i++) {
		_function->mask(i) = fix[i];
	}
}

const Vector<Bool>& SpectralElement::fixed() const {
	return _fixed;
}

Bool SpectralElement::toRecord(RecordInterface &out) const {
	out.define(RecordFieldId("type"), fromType(_type));
	Vector<Double> ptmp(_params);
	Vector<Double> etmp(_params);
	out.define(RecordFieldId("parameters"), _params);
	out.define(RecordFieldId("errors"), _errors);
	out.define(RecordFieldId("fixed"), _fixed);
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

} //# NAMESPACE CASA - END


