//# MVuvw.cc: A 3D vector on Earth
//# Copyright (C) 1998
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
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_mvuvw_bug1;
#endif
#include <aips/Measures/MVuvw.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/RTTI/Register.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/Euler.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/QMath.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MVBaseline.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

// MVuvw class

//# Constructors
MVuvw::MVuvw() :
  MVPosition() {}

MVuvw &MVuvw::operator=(const MVuvw &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVuvw::MVuvw(Double in) :
  MVPosition(in) {}

MVuvw::MVuvw(const Quantity &l) :
  MVPosition(l) {}

MVuvw::MVuvw(Double in0, Double in1, Double in2) : 
  MVPosition(in0, in1, in2) {}

MVuvw::MVuvw(const Quantity &l, Double angle0, Double angle1) : 
  MVPosition(l, angle0, angle1) {}

MVuvw::MVuvw(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  MVPosition(l, angle0, angle1) {}

MVuvw::MVuvw(const Quantum<Vector<Double> > &angle) :
  MVPosition(angle) {}

MVuvw::MVuvw(const Quantity &l, 
		       const Quantum<Vector<Double> > &angle) :
  MVPosition(l, angle) {}

MVuvw::MVuvw(const Vector<Double> &other) :
  MVPosition(other) {}

MVuvw::MVuvw(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantum vector in MVuvw constructor"));
    };
  }

MVuvw::MVuvw(const MVBaseline &pos, const MVDirection &dr, Bool ew) :
  MVPosition() {
    RotMatrix x(Euler(-(C::pi_2 - dr.get()(1)), 1,
		      (dr.get()(0) - C::pi_2), 3));
    if (ew) {};
    xyz = (x * pos).getValue();
  }

MVuvw::MVuvw(const MVPosition &other) :
  MVPosition(other) {}

//# Destructor
MVuvw::~MVuvw() {}

//# Operators
Bool MVuvw::
operator==(const MVuvw &other) const {
  return (allEQ(xyz.ac(), other.xyz.ac()));
}

Bool MVuvw::
operator!=(const MVuvw &other) const {
  return ToBool(!(*this == other));
}

Bool MVuvw::
near(const MVuvw &other, Double tol) const {
  return (allNear(xyz.ac(), other.xyz.ac(), tol));
}

Bool MVuvw::
near(const MVuvw &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVuvw::
nearAbs(const MVuvw &other, Double tol) const {
  return (allNearAbs(xyz.ac(), other.xyz.ac(), tol));
}

Double MVuvw::
operator*(const MVuvw &other) const {
  Double tmp = 0.0;
  for (Int i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

MVuvw MVuvw::operator-() const {
  MVuvw tmp; tmp = *this;
  tmp.xyz.ac() = -xyz.ac();
  return tmp;
}

MVuvw &MVuvw::operator+=(const MVuvw &right) {
  xyz.ac() += right.xyz.ac();
  return *this;
}

MVuvw MVuvw::operator+(const MVuvw &right) const {
  MVuvw tmp = *this;
  tmp += right;
  return tmp;
}

MVuvw &MVuvw::operator-=(const MVuvw &right) {
  xyz.ac() -= right.xyz.ac();
  return *this;
}

MVuvw MVuvw::operator-(const MVuvw &right) const{
  MVuvw tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

uInt MVuvw::type() const {
  return Register((MVuvw *)0);
}

void MVuvw::assert(const MeasValue &in) {
  if (in.type() != Register((MVuvw *)0)) {
    throw(AipsError("Illegal MeasValue type argument: MVuvw"));
  };
}

void MVuvw::adjust() {}

void MVuvw::adjust(Double &res) {
  res = sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz.ac() /= res;
  };
}

void MVuvw::readjust(Double res) {
  if (res == 0.0) {
    xyz.ac() *= 1e-12;
  } else {
    xyz.ac() *= res;
  };
}

Double MVuvw::radius() {
  return (sqrt(operator*(*this)));
}

Vector<Double> MVuvw::get() const{
  Vector<Double> tmp(3);
  tmp(0) = sqrt(operator*(*this));
  Double ln = (tmp(0) == 0.0 ? 1.0 : tmp(0));
  Double loc = xyz(0)/ln;
  if (loc == 0) {
    tmp(1) = asin(xyz(1)/ln);
  } else {
    tmp(1) = atan2(xyz(1),xyz(0));
  };
  tmp(2) = asin(xyz(2)/ln);
  return tmp;
}

const Vector<Double> &MVuvw::getValue() const {
  return xyz;
}

Quantum<Vector<Double> > MVuvw::getAngle() const{
  Vector<Double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<Double> >(tmp,"rad");
}

Quantum<Vector<Double> > MVuvw::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVuvw::getLength() const{
  Double tmp = sqrt(operator*(*this));
  return Quantity(tmp,"m");
}

Quantity MVuvw::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVuvw::uvwAngle(const MVuvw &other) const {
  Vector<Double> t1(3);
  Vector<Double> t2(3);
  t1 = get();
  t2 = other.get();
  Double s1,c1;
  c1 = cos(t1(2)) * sin(t2(2)) -
    sin(t1(2)) * cos(t2(2)) * cos(t1(1) - t2(1));
  s1 = -cos(t2(2)) * sin(t1(1) - t2(1));
  if (s1 != 0 || c1 != 0) {
    return atan2(s1, c1);
  } else {
    return Double(0.0);
  };
}

Quantity MVuvw::uvwAngle(const MVuvw &other, 
				   const Unit &unit) const {
  return Quantity(uvwAngle(other), "rad").get(unit);
}

Double MVuvw::separation(const MVuvw &other) const {
  MVuvw t1(*this);
  MVuvw t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  Double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*asin(d1));
}

Quantity MVuvw::separation(const MVuvw &other, 
				const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

MVuvw MVuvw::crossProduct(const MVuvw &other) const {
  MVuvw res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

void MVuvw::print(ostream &os) const {
  os << getValue().ac();
}

MeasValue *MVuvw::clone() const {
  return (new MVuvw(*this));
}

Vector<Double> MVuvw::getVector() const {
  return xyz;
}

void MVuvw::putVector(const Vector<Double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uInt i=0; i<in.nelements();i++) xyz(i) = in(i);
  };
}

Vector<Quantum<Double> > MVuvw::getRecordValue() const {
  Vector<Double> t(3);
  t = get();
  Vector<Quantum<Double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<Double> > MVuvw::getXRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

Bool MVuvw::putValue(const Vector<Quantum<Double> > &in) {
  uInt i = in.nelements();
  if (i != 3 ) return False;
  if (in(0).check(UnitVal::LENGTH)) {
    if (in(1).check(UnitVal::LENGTH) &&
	in(2).check(UnitVal::LENGTH)) {
      for (uInt j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      };
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      for (uInt j=1; j < i; j++) {
	tsin(j-1) = (sin(in(j))).getValue(); 
	tcos(j-1) = (cos(in(j))).getValue(); 
      };
      xyz = Double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(0).getBaseValue());
    } else {
      return False;
    };
  } else if (in(2).check(UnitVal::LENGTH)) {
    if (in(0).check(UnitVal::ANGLE) &&
	in(1).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      Int j;
      for (j=0; j < 2; j++) {
	tsin(j) = (sin(in(j))).getValue(); 
	tcos(j) = (cos(in(j))).getValue(); 
      };
      xyz = Double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(2).getBaseValue());
    } else {
      return False;
    };
  };
  return True;
}

MVuvw operator*(const RotMatrix &left, const MVuvw &right) {
  MVuvw result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVuvw operator*(const MVuvw &left, const RotMatrix &right) {
  MVuvw result(left);
  result *= right;
  return result;
}

MVuvw operator*(Double left, const MVuvw &right) {
  MVuvw result(right);
  result *= left;
  return result;
}

MVuvw operator*(const MVuvw &left, Double right) {
  MVuvw result(left);
  result *= right;
  return result;
}

Double operator*(const Vector<Double> &left, const MVuvw &right) {
  MVuvw tmp(left);
  return (tmp * right);
}

Double operator*(const MVuvw &left, const Vector<Double> &right) {
  MVuvw tmp(right);
  return (tmp * left);
}

Double operator*(const MVPosition &left, const MVuvw &right) {
  MVuvw tmp(left);
  return (tmp * right);
}

Double operator*(const MVuvw &left, const MVPosition &right) {
  MVuvw tmp(right);
  return (tmp * left);
}
