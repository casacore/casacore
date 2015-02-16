//# QLogical.cc: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1997,1998,1999
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

#ifndef CASA_QLOGICAL_TCC
#define CASA_QLOGICAL_TCC

//# Includes
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue() ){
	Qtype tmp;
	tmp = other.get(left.getFullUnit()).getValue();
	return QMakeBool((left.getValue()) == (tmp));
    }
    return False;
}

template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(left == loc);
}

template <class Qtype>
Bool operator==(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(loc == other);
}

template <class Qtype>
Bool operator!=(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left == other));
}

template <class Qtype>
Bool operator!=(const Quantum<Qtype> &left, const Qtype &other) {
    return QMakeBool(!(left == other));
}

template <class Qtype>
Bool operator!=(const Qtype &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left == other));
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
      return QMakeBool(near(left.getValue(),
			    other.get(left.getFullUnit()).getValue()));
    }
    return False;
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol) {
    UnitVal kind, knew;
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
      return QMakeBool(near(left.getValue(),
			    other.get(left.getFullUnit()).getValue(),tol));
    }
    return False;
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(near(left, loc));
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other, Double tol) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(near(left, loc, tol));
}

template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(near(loc, other));
}

template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other, Double tol) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(near(loc, other, tol));
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
      return QMakeBool(nearAbs(left.getValue(),
			       other.get(left.getFullUnit()).getValue()));
    }
    return False;
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
	return QMakeBool(nearAbs(left.getValue(),
				 other.get(left.getFullUnit()).getValue(),tol));
    }
    return False;
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  const Quantum<Qtype>& tol) {
	if (left.getFullUnit().getValue() == tol.getFullUnit().getValue()) {
		return nearAbs(
			left.get(tol.getUnit()), other.get(tol.getUnit()),
			tol.getValue()
		);
	}
    return False;
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(nearAbs(left, loc));
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other, Double tol) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(nearAbs(left, loc, tol));
}

template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(nearAbs(loc, other));
}

template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other, Double tol) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(nearAbs(loc, other, tol));
}

template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() != other.getFullUnit().getValue()) {
	throw (AipsError("Quantum::operator< unequal units '" +
			 left.getUnit() + ", '" + other.getUnit() + "'"));
    } else {
      return QMakeBool(QMakeBool(left.getValue() <
				 other.get(left.getFullUnit()).getValue()));
    }
    return False;
}

template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(left < loc);
}

template <class Qtype>
Bool operator<(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(loc < other);
}

template <class Qtype>
Bool operator>(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() != other.getFullUnit().getValue()) {
	throw (AipsError("Quantum::operator< unequal units '" +
			 left.getUnit() + ", '" + other.getUnit() + "'"));
    } else {
	return QMakeBool(left.getValue() >
			 other.get(left.getFullUnit()).getValue());
    }
    return False;
}

template <class Qtype>
Bool operator>(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return QMakeBool(left > loc);
}

template <class Qtype>
Bool operator>(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return QMakeBool(loc > other);
}

template <class Qtype>
Bool operator<=(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left > other));
}

template <class Qtype>
Bool operator<=(const Quantum<Qtype> &left, const Qtype &other) {
    return QMakeBool(!(left > other));
}

template <class Qtype>
Bool operator<=(const Qtype &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left > other));
}

template <class Qtype>
Bool operator>=(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left < other));
}

template <class Qtype>
Bool operator>=(const Quantum<Qtype> &left, const Qtype &other) {
    return QMakeBool(!(left < other));
}

template <class Qtype>
Bool operator>=(const Qtype &left, const Quantum<Qtype> &other) {
    return QMakeBool(!(left < other));
}


} //# NAMESPACE CASACORE - END


#endif
