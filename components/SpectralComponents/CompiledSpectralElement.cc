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

#include <components/SpectralComponents/CompiledSpectralElement.h>

#include <scimath/Functionals/CompiledFunction.h>
#include <scimath/Mathematics/AutoDiffMath.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

CompiledSpectralElement::CompiledSpectralElement() {}

CompiledSpectralElement::CompiledSpectralElement(
		const String& function,
		const Vector<Double>& param
) : SpectralElement(SpectralElement::COMPILED, param)/*, _function(function)*/ {
	_setFunction(function);
	/*
	if (! comp->setFunction(_function)) {
		throw AipsError(
			"CompiledSpectralElement: An illegal functional string "
			"was specified for a compiled SpectralElement"
		);
	}
	if(comp->nparameters() != param.size()) {
		throw AipsError(
			"CompiledSpectralElement: Number of parameters in the "
			"compiled function does not match number of input parameters"
		);
	}
	*/
}

CompiledSpectralElement::CompiledSpectralElement(
	SpectralElement::Types type, const Vector<Double>& param
) : SpectralElement(type, param)/*, _function("")*/ {}

CompiledSpectralElement::CompiledSpectralElement(
	SpectralElement::Types type, uInt nParam
) : SpectralElement(type, Vector<Double>(nParam))/*, _function("")*/ {}

CompiledSpectralElement::CompiledSpectralElement(
	const CompiledSpectralElement& other
) : SpectralElement(other)/*, _function(other._function)*/ {}

CompiledSpectralElement::~CompiledSpectralElement() {}

SpectralElement* CompiledSpectralElement::clone() const {
	return new CompiledSpectralElement(*this);
}

CompiledSpectralElement& CompiledSpectralElement::operator=(
	const CompiledSpectralElement &other
) {
	if (this != &other) {
		SpectralElement::operator=(other);
		//_function = other._function;
	}
	return *this;
}

/*
Bool CompiledSpectralElement::operator==(
	const CompiledSpectralElement& other
) const {
	return SpectralElement::operator==(other);// && _function == other._function;

}
*/

/*
Double CompiledSpectralElement::operator()(const Double x) const {
	CompiledFunction<Double> comp;
	comp.setFunction(_function);
	comp.parameters().setParameters(get());
	return comp(x);
}
*/

const String& CompiledSpectralElement::getFunction() const {
	// return _function;
	std::tr1::shared_ptr<Function<Double, Double> >  f = _getFunction();
	return dynamic_cast<CompiledFunction<Double> *>(SpectralElement::_getFunction().get())->getText();
}


void CompiledSpectralElement::_setFunction(const String& function) {
	//_function = function;
	std::tr1::shared_ptr<Function<Double, Double> > f = _getFunction();
	CompiledFunction<Double> *cf = f.get() == 0
		? new CompiledFunction<Double>()
		: dynamic_cast<CompiledFunction<Double> *>(f.get());
	if (! cf->setFunction(function)) {
		throw AipsError(
			"CompiledSpectralElement: An illegal functional string "
			"was specified for a compiled SpectralElement"
		);
	}
	if (f.get() == 0) {
		f.reset(cf);
	}
	uInt n = get().size();
	if(n > 0 && cf->nparameters() != n) {
		throw AipsError(
			"CompiledSpectralElement: Number of parameters in the "
			"compiled function does not match number of input parameters"
		);
	}
	SpectralElement::_setFunction(f);
}


Bool CompiledSpectralElement::toRecord(RecordInterface& out) const {
	SpectralElement::toRecord(out);
	out.define(RecordFieldId("compiled"), getFunction());
	return True;
}

ostream &operator<<(ostream& os, const CompiledSpectralElement& elem) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
	os << "  Function:    " << elem.getFunction() << endl;
	const Vector<Double> p = elem.get();
	for (uInt i=0; i<p.size(); i++) {
		os << "p" << i << ": " << p[i] << endl;
	}
	return os;
}

} //# NAMESPACE CASA - END


