//# Polynomial2.cc: One dimensional polynomial class specialized for AutoDiff
//# Copyright (C) 2001,2002
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
//# $Id: Polynomial2.tcc 20253 2008-02-23 15:15:00Z gervandiepen $

//# Includes
#include <scimath/Functionals/PowerLogarithmicPolynomial.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constructors

//# Operators
template<class T>
AutoDiff<T> PowerLogarithmicPolynomial<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
	AutoDiff<T> tmp;
	for (uInt i=0; i<this->nparameters(); ++i) {
		if (this->param_p[i].nDerivatives() > 0) {
			tmp = this->param_p[i];
			break;
		}
	}
	// function value
	T lnx = log(x[0]);
	Int j = nparameters();
	T accum = this->param_p[--j].value();
	while (--j >= 1) {
		accum *= lnx;
		accum += param_p[j].value();
	}
	T value = param_p[0].value()*pow(x[0], accum);
	tmp.value() = value;
	// get derivatives

	Double prod = value;
	if (tmp.nDerivatives() > 0) {
		for (uInt j=0; j<tmp.nDerivatives(); j++) {
			tmp.deriv(j) = 0.0;
		}
		for (uInt i=0; i<this->nparameters(); ++i) {
			if (i == 0 && this->param_p.mask(0)) {
				tmp.deriv(0) = value/this->param_p[0].value();
			}
			else {
				prod *= lnx;
				if (this->param_p.mask(i)) {
					tmp.deriv(i) = prod;
				}
			}
		}
	}
	return tmp;
}

//# Member functions

} //# NAMESPACE CASA - END

