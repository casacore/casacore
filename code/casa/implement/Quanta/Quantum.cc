//# Quantum.cc: class to manipulate physical, dimensioned quantities
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
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MUString.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVTime.h>
#include <aips/RTTI/Register.h>

#if defined(__GNUG__)
typedef Quantum<Double> gpp_bug_1;
#endif

template <class Qtype>
Quantum<Qtype>::Quantum() :
    QBase() { qVal = (Qtype)0;}

template <class Qtype>
Quantum<Qtype>::Quantum(const Quantum<Qtype> &other) :
    QBase(other), qVal(other.qVal) {}

template <class Qtype>
Quantum<Qtype>::Quantum(const Qtype &factor) : QBase(),
    qVal(factor) {}

template <class Qtype>
Quantum<Qtype>::Quantum(const Qtype &factor, const Unit &s) :
                   QBase(s),
		   qVal(factor) {}

template <class Qtype>
Quantum<Qtype>::Quantum(const Qtype &factor, const QBase &other) :
                   QBase(other),
                   qVal(factor) {}

template <class Qtype>
Quantum<Qtype>::~Quantum() {}

//# Quantum operators

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator=(const Quantum<Qtype> &other) {
    if (this != &other) {
	qVal=other.qVal;
	qUnit=other.qUnit;
    }
    return *this;
}

template <class Qtype>
const Quantum<Qtype> &Quantum<Qtype>::operator+() const{
    return *this;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::operator-() const{
    Quantum<Qtype> loc;
    loc.qVal = -at_cc(qVal);
    loc.qUnit = qUnit;
    return loc;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator+=(const Quantum<Qtype> &other) {
    if (qUnit.getValue() != other.qUnit.getValue()) {
	throw (AipsError("Quantum::operator+ unequal units '" +
			 qUnit.getName() + ", '" + 
			 other.qUnit.getName() + "'"));
    } else {
        Qtype tmp = other.getValue(qUnit);
	at_c(qVal) += at_cc(tmp);
    }
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator+=(const Qtype &other) {
    at_c(qVal) += at_cc(other);
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator-=(const Quantum<Qtype> &other) {
    if (qUnit.getValue() != other.qUnit.getValue()) {
	throw (AipsError("Quantum::operator- unequal units '" +
			 qUnit.getName() + ", '" + 
			 other.qUnit.getName() + "'"));
    } else {
        Qtype tmp = other.getValue(qUnit);
	at_c(qVal) -= at_cc(tmp);
    }
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator-=(const Qtype &other) {
    at_c(qVal) -= at_cc(other);
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator*=(const Quantum<Qtype> &other) {
    at_c(qVal) *= at_cc(other.qVal); 
    if (!(other.qUnit.getName().empty())) {
	if (qUnit.getName().empty()) {
	    qUnit = other.qUnit;
	} else {
	    qUnit = Unit(qUnit.getName() + ("." + other.qUnit.getName()));
	}
    }
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator*=(const Qtype &other) {
    at_c(qVal) *= at_cc(other);
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator/=(const Quantum<Qtype> &other) {
    at_c(qVal) /= at_cc(other.qVal);
    if (!(other.qUnit.getName().empty())) {
	if (qUnit.getName().empty()) {
	    qUnit = Unit(String("(") + other.qUnit.getName() +
			 String(")-1"));
	} else {
	    qUnit = Unit(qUnit.getName() + ("/(" + 
					    other.qUnit.getName() + ")"));
	}
    }
    return *this;
}

template <class Qtype>
Quantum<Qtype> &Quantum<Qtype>::operator/=(const Qtype &other) {
    at_c(qVal) /= at_cc(other);
    return *this;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::operator+(const Quantum<Qtype> &other) const{
    Quantum<Qtype> loc; loc = *this;
    loc += other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::operator-(const Quantum<Qtype> &other) const{
    Quantum<Qtype> loc; loc = *this;
    loc -= other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::operator*(const Quantum<Qtype> &other) const{
    Quantum<Qtype> loc; loc = *this; 
    loc *= other;
    return loc;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::operator/(const Quantum<Qtype> &other) const{
    Quantum<Qtype> loc; loc = *this;
    loc /= other;
    return loc;
}

template <class Qtype>
void  Quantum<Qtype>::print(ostream &os) const {
    os << at_cc(qVal) << " " << qUnit.getName();
}

//# Quantum general member functions

template <class Qtype>
const Qtype &Quantum<Qtype>::getValue() const {
    return qVal;
}

template <class Qtype>
Qtype & Quantum<Qtype>::getValue() {
  return qVal;
}

template <class Qtype>
Qtype Quantum<Qtype>::getValue(const Unit &other) const {
    Double d1 = other.getValue().getFac() /
	qUnit.getValue().getFac();	// SUN native overloading problems
    if (qUnit.getValue() == UnitVal::ANGLE) {
      if (other.getValue() == UnitVal::TIME)
	d1 *= C::circle/C::day;
    } else if (qUnit.getValue() == UnitVal::TIME) {
      if (other.getValue() == UnitVal::ANGLE)
	d1 *= C::day/C::circle;
    };
    return (Qtype)(at_cc(qVal)/d1);
}

template <class Qtype>
Qtype Quantum<Qtype>::getBaseValue() const {
    return (Qtype)(at_cc(qVal) * qUnit.getValue().getFac());
}

template <class Qtype>
const Unit &Quantum<Qtype>::getFullUnit() const {
    return qUnit;
}

template <class Qtype>
void Quantum<Qtype>::scale(const Qtype &factor) {
    at_c(qVal) *= at_cc(factor);
}

template <class Qtype>
void Quantum<Qtype>::setValue(const Qtype &val) {
    qVal = val;
}

template <class Qtype>
Bool Quantum<Qtype>::read(Quantity &res, MUString &in) {
  Double val0 = 0.0;
  String unit = "";
  res = Quantity();
  UnitVal uv;
  in.push();
  if (!in.eos()) {
    if (MVAngle::read(res, in) || MVTime::read(res, in)) {
      val0 = res.getValue();
      unit = res.getUnit();
    } else {
      val0 = in.getDouble();
      unit = in.get();
      // Check if valid unit specified
      if (!UnitVal::check(unit, uv)) {
	in.pop(); return False;
      };
    };
  };
  //
  // The next statement is necessary once the read return arg is templated
  //  Qtype tmp = (Qtype)(at_cc(res.getValue()) + val0)
  res.setValue(val0);
  res.setUnit(unit);
  in.unpush(); return True; 
}

template <class Qtype>
Bool Quantum<Qtype>::read(Quantity &res, const String &in) {
  MUString tmp(in);		// Pointed non-const String
  return Quantum<Qtype>::read(res, tmp);
}

template <class Qtype>
Bool Quantum<Qtype>::check(const UnitVal &uv) const {
    return ( (qUnit.getValue() == uv) ? True : False); 
}

template <class Qtype>
void Quantum<Qtype>::assert(const UnitVal &uv) const {
    if (qUnit.getValue() != uv) {
	throw(AipsError("Quantum::assert non-conforming unit type '" +
			getUnit() + "'"));
    }
}

template <class Qtype>
void Quantum<Qtype>::convert() {
    this->convert(Unit());
}

template <class Qtype>
void Quantum<Qtype>::convert(const Unit &s) {
    if (qUnit.getValue() == s.getValue()) {
      // To suppress some warnings, next statement not used
      //	at_c(qVal) *= (qUnit.getValue().getFac()/s.getValue().getFac());
      at_c(qVal) = (Qtype) (at_cc(qVal) * 
			    qUnit.getValue().getFac()/s.getValue().getFac());
      qUnit = s;
    } else {
      if (qUnit.getValue() == UnitVal::ANGLE && 
	  s.getValue() == UnitVal::TIME) {
	at_c(qVal) = (Qtype) (at_cc(qVal) *
			      qUnit.getValue().getFac()/
			      s.getValue().getFac() * C::day/C::circle);
	qUnit = s;
      } else if (qUnit.getValue() == UnitVal::TIME &&
		 s.getValue() == UnitVal::ANGLE) {
	at_c(qVal) = (Qtype) (at_cc(qVal) *
			      qUnit.getValue().getFac()/
			      s.getValue().getFac() * C::circle/C::day);
	qUnit = s;
      } else {
	qUnit.setValue(qUnit.getValue() / s.getValue());
	ostrstream oss;
	oss << qUnit.getValue().getDim();
	// Suppress (gcc) warnings:
	at_c(qVal) = (Qtype) (at_cc(qVal) * 
			      qUnit.getValue().getFac());
	if (s.empty()) {
	  qUnit = String(oss);
	} else {
	  qUnit = Unit(s.getName() + '.' + String(oss).after(0));
	};
      };
    };
}

template <class Qtype>
void Quantum<Qtype>::convert(const Quantum<Qtype> &other) {
    this->convert(other.qUnit);
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::get() const {
    return get(Unit());
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::get(const Unit &s) const {
    Quantum<Qtype> res = *this;
    res.convert(s); return res;
}

template <class Qtype>
Quantum<Qtype> Quantum<Qtype>::get(const Quantum<Qtype> &other) const{
    return get(other.qUnit);
}

template <class Qtype>
QBase *Quantum<Qtype>::clone() const {
  return (new Quantum<Qtype>(*this));
}

template <class Qtype>
uInt Quantum<Qtype>::type() const {
  return Register((Quantum<Qtype> *)0);
}

template <class Qtype>
uInt Quantum<Qtype>::myType() {
  return Register((Quantum<Qtype> *)0);
}

