//# QLogical.cc: class to manipulate physical, dimensioned quantities
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
#include <aips/Quanta/QLogical.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>

#ifdef __GNUG__
    typedef Array<Bool> gnu_array_bool;
#endif

template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue() ){
	Qtype tmp;
	tmp = other.get(left.getFullUnit()).getValue();
	return QMakeBool(at_cc(left.getValue()) == at_cc(tmp));
    }
    return False;
}

template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return (left == loc);
}

template <class Qtype>
Bool operator==(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return (loc == other);
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
	return near(left.getValue(),
		    other.get(left.getFullUnit()).getValue());
    }
    return False;
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol) {
    UnitVal kind, knew;
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
	return near(left.getValue(),
		    other.get(left.getFullUnit()).getValue(),tol);
    }
    return False;
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return near(left, loc);
}

template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other, Double tol) {
    Quantum<Qtype> loc; loc = other;
    return near(left, loc, tol);
}

template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return near(loc, other);
}

template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other, Double tol) {
    Quantum<Qtype> loc; loc = left;
    return near(loc, other, tol);
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
	return nearAbs(left.getValue(),
		       other.get(left.getFullUnit()).getValue());
    }
    return False;
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol) {
    if (left.getFullUnit().getValue() == other.getFullUnit().getValue()) {
	return nearAbs(left.getValue(),
		       other.get(left.getFullUnit()).getValue(),tol);
    }
    return False;
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return nearAbs(left, loc);
}

template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other, Double tol) {
    Quantum<Qtype> loc; loc = other;
    return nearAbs(left, loc, tol);
}

template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return nearAbs(loc, other);
}

template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other, Double tol) {
    Quantum<Qtype> loc; loc = left;
    return nearAbs(loc, other, tol);
}

template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
    if (left.getFullUnit().getValue() != other.getFullUnit().getValue()) {
	throw (AipsError("Quantum::operator< unequal units '" +
			 left.getUnit() + ", '" + other.getUnit() + "'"));
    } else {
	return QMakeBool(left.getValue() <
			 other.get(left.getFullUnit()).getValue());
    }
    return False;
}

template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Qtype &other) {
    Quantum<Qtype> loc; loc = other;
    return (left < loc);
}

template <class Qtype>
Bool operator<(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return (loc < other);
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
    return (left > loc);
}

template <class Qtype>
Bool operator>(const Qtype &left, const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = left;
    return (loc > other);
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

