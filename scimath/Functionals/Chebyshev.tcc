//# Chebyshev.cc  a function class that defines a Chebyshev polynomial
//# Copyright (C) 2000,2001,2002
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

#ifndef SCIMATH_CHEBYSHEV_TCC
#define SCIMATH_CHEBYSHEV_TCC

//# Includes
#include <casacore/scimath/Functionals/Chebyshev.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template <class T>
T Chebyshev<T>::eval(const typename FunctionTraits<T>::ArgType *x) const {
    T xp = x[0];
    // handle out-of-interval values
    if (xp < this->minx_p || xp > this->maxx_p) {
	switch (this->mode_p) {

	case ChebyshevEnums::CONSTANT:
	    return this->def_p;

	case ChebyshevEnums::ZEROTH:
	    return this->param_p[0];

	case ChebyshevEnums::CYCLIC: {
	    T period = (this->maxx_p-this->minx_p);
	    while (xp < this->minx_p) xp += period;
	    while (xp > this->maxx_p) xp -= period;
	}
	break;

	case ChebyshevEnums::EDGE: {
	    T tmp(0);
	    if (xp<this->minx_p) {
		for (uInt i=0; i<this->nparameters(); i+=2) tmp += this->param_p[i];
		for (uInt i=1; i<this->nparameters(); i+=2) tmp -= this->param_p[i];
	    } else {
		for (uInt i=0; i<this->nparameters(); ++i) tmp += this->param_p[i];
	    }
	    return tmp;
	}
	break;

	default:
	    break;
	}
    }

    T yi1=T(0);
    T yi2=T(0);
    T tmp;

    // map Chebeshev range [this->minx_p, this->maxx_p] into [-1, 1]
    xp = (T(2)*xp-this->minx_p-this->maxx_p)/(this->maxx_p-this->minx_p);

    // evaluate using Clenshaw recursion relation
    for (Int i=this->nparameters()-1; i>0; i--) {
	tmp = T(2)*xp*yi1 - yi2 + this->param_p[i];
	yi2 = yi1;
	yi1 = tmp;
    }

    return xp*yi1 - yi2 + this->param_p[0];
}

template <class T>
Chebyshev<T> Chebyshev<T>::derivative() const {
    Vector<T> ce(this->nparameters());
    ce = this->parameters().getParameters();
    this->derivativeCoeffs(ce, this->minx_p, this->maxx_p);
    return Chebyshev<T>(ce, this->minx_p, this->maxx_p, this->mode_p, T(0));
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
