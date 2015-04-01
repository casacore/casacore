//# QMath.cc: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2003
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

#ifndef CASA_QMATH_TCC
#define CASA_QMATH_TCC

//# Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class Qtype>
Quantum<Qtype> operator+(const Quantum<Qtype> &left,
			 const Qtype &other) {
    Quantum<Qtype> loc; loc = left;
    loc += other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator+(const Qtype &left,
			 const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = other;
    loc += left;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator-(const Quantum<Qtype> &left,
			 const Qtype &other) {
    Quantum<Qtype> loc; loc = left;
    loc -= other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator-(const Qtype &left,
			 const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = other;
    loc -= left;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator*(const Quantum<Qtype> &left,
			 const Qtype &other) {
    Quantum<Qtype> loc = left;
    loc *= other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator*(const Qtype &left,
			 const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = other;
    loc *= left;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator/(const Quantum<Qtype> &left,
			 const Qtype &other) {
    Quantum<Qtype> loc = left;
    loc /= other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> operator/(const Qtype &left,
			 const Quantum<Qtype> &other) {
    Quantum<Qtype> loc; loc = other;
    loc /= left;
    return loc;
}

// pow. Implemented as a repeated multiplication/division:
// - to cater for all data types
// - only Int powers allowed
// - limited values of exponentials foreseen
template <class Qtype>
Quantum<Qtype> pow(const Quantum<Qtype> &left, Int p) {
    if (::abs(p) >= 100) throw (AipsError("Quantum::pow exponent too large")); 
// Make sure 1 in current data type available
    Quantum<Qtype> res;
    Qtype tmp; tmp = left.getValue() * 0. + 1.;

    Int i;
    if (p>=0) {
	for (i=0; i<p; i++) tmp *= left.getValue();
    } else {
	for (i=0; i>p; i--) tmp /= left.getValue();
    }
    res.setValue(tmp);
    if (p == 0 || left.getUnit().empty()) res.setUnit("");
    else {
      String sloc = "(" + left.getUnit() + ")";
      if (p < 0) {
	sloc += "-";
	p = -p;
      }
      if (p/10 != 0) sloc += Char(Int(p)/10 + '0');
      sloc += Char(Int(p)%10 + '0');
      res.setUnit(sloc);
    }
    return res;
}

template <class Qtype>
Quantum<Qtype> root(const Quantum<Qtype> &left, Int p) {
  if (p == 0) throw (AipsError("Quantum::root exponent zero"));
  Quantum<Qtype> res;
  res.setValue(casacore::pow(left.getValue(), 1.0/Double(p)));
  UnitVal vres(left.getFullUnit().getValue().root(p));
  ostringstream oss;
  oss << vres.getDim();
  res.setUnit(String(oss));
  res.setValue(res.getValue() * vres.getFac());
  return res;
}

template <class Qtype>
Quantum<Qtype> sqrt(const Quantum<Qtype> &left) {
  return root(left, 2);
}

template <class Qtype>
Quantum<Qtype> abs(const Quantum<Qtype> &left) {
    Qtype tmp = left.getValue(); 
    Qtype ret;
    ret = abs((tmp));
    return (Quantum<Qtype>(ret,left));
}

template <class Qtype>
Quantum<Qtype> ceil(const Quantum<Qtype> &left) {
    Qtype tmp = left.getValue(); 
    Qtype ret;
    ret = ceil((tmp));
    return (Quantum<Qtype>(ret,left));
}

template <class Qtype>
Quantum<Qtype> floor(const Quantum<Qtype> &left) {
    Qtype tmp = left.getValue(); 
    Qtype ret;
    ret = floor((tmp));
    return (Quantum<Qtype>(ret,left));
}

template <class Qtype>
Quantum<Qtype> sin(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::ANGLE) {
	throw (AipsError("Quantum::sin illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(sin((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> cos(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::ANGLE) {
	throw (AipsError("Quantum::cos illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(cos((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> tan(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::ANGLE) {
	throw (AipsError("Quantum::tan illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(tan((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> asin(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::asin illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(asin((res.getValue())));
    res.setUnit("rad");
    return (res);
}

template <class Qtype>
Quantum<Qtype> acos(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::acos illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(acos((res.getValue())));
    res.setUnit("rad");
    return (res);
}

template <class Qtype>
Quantum<Qtype> atan(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::atan illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(atan((res.getValue())));
    res.setUnit("rad");
    return (res);
}

template <class Qtype>
Quantum<Qtype> atan2(const Quantum<Qtype> &left,
		     const Quantum<Qtype> &other) {
    if ((left.getFullUnit().getValue() != UnitVal::NODIM) ||
	(other.getFullUnit().getValue() != UnitVal::NODIM)) {
	throw (AipsError("Quantum::atan2 illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    Qtype tmp; tmp = other.getBaseValue();
    res.setValue(left.getBaseValue());
    res.setValue(atan2((res.getValue()),(tmp)));
    res.setUnit("rad");
    return (res);
}

template <class Qtype>
Quantum<Qtype> atan2(const Quantum<Qtype> &left,
		     const Qtype &other) {
    Quantum<Qtype> res; res = other;
    return (atan2(left,res));
}

template <class Qtype>
Quantum<Qtype> atan2(const Qtype &left,
		     const Quantum<Qtype> &other) {
    Quantum<Qtype> res; res = left;
    return (atan2(res,other));
}

template <class Qtype>
Quantum<Qtype> log(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::log illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(log((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> log10(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::log10 illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(log10((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> exp(const Quantum<Qtype> &left) {
    if (left.getFullUnit().getValue() != UnitVal::NODIM) {
	throw (AipsError("Quantum::exp illegal unit type '" +
			 left.getUnit() + "'"));
    }
    Quantum<Qtype> res;
    res.setValue(left.getBaseValue());
    res.setValue(exp((res.getValue())));
    res.setUnit("");
    return (res);
}

template <class Qtype>
Quantum<Qtype> min(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
	return left < other ? left : other;
}

template <class Qtype>
Quantum<Qtype> max(const Quantum<Qtype> &left, const Quantum<Qtype> &other) {
	return left > other ? left : other;
}

} //# NAMESPACE CASACORE - END


#endif
