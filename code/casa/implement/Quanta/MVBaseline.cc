//# MVBaseline.cc: A 3D vector on Earth
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
typedef Quantum<Double> gpp_mvBaseline_bug1;
#endif
#include <aips/Quanta/MVBaseline.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/QMath.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

// MVBaseline class

//# Constructors
MVBaseline::MVBaseline() :
  MVPosition() {}

MVBaseline &MVBaseline::operator=(const MVBaseline &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVBaseline::MVBaseline(Double in) :
  MVPosition(in) {}

MVBaseline::MVBaseline(const Quantity &l) :
  MVPosition(l) {}

MVBaseline::MVBaseline(Double in0, Double in1, Double in2) : 
  MVPosition(in0, in1, in2) {}

MVBaseline::MVBaseline(const Quantity &l, Double angle0, Double angle1) : 
  MVPosition(l, angle0, angle1) {}

MVBaseline::MVBaseline(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  MVPosition(l, angle0, angle1) {}

MVBaseline::MVBaseline(const Quantum<Vector<Double> > &angle) :
  MVPosition(angle) {}

MVBaseline::MVBaseline(const Quantity &l, 
		       const Quantum<Vector<Double> > &angle) :
  MVPosition(l, angle) {}

MVBaseline::MVBaseline(const Vector<Double> &other) :
  MVPosition(other) {}

MVBaseline::MVBaseline(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantum vector in MVBaseline constructor"));
    };
  }

MVBaseline::MVBaseline(const MVPosition &pos, const MVPosition &base) :
  MVPosition(pos) {
    xyz.ac() -= base.getValue().ac();
}

MVBaseline::MVBaseline(const MVPosition &other) :
  MVPosition(other) {}

//# Destructor
MVBaseline::~MVBaseline() {}

//# Operators
Bool MVBaseline::
operator==(const MVBaseline &other) const {
  return (allEQ(xyz.ac(), other.xyz.ac()));
}

Bool MVBaseline::
operator!=(const MVBaseline &other) const {
  return ToBool(!(*this == other));
}

Bool MVBaseline::
near(const MVBaseline &other, Double tol) const {
  return (allNear(xyz.ac(), other.xyz.ac(), tol));
}

Bool MVBaseline::
near(const MVBaseline &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVBaseline::
nearAbs(const MVBaseline &other, Double tol) const {
  return (allNearAbs(xyz.ac(), other.xyz.ac(), tol));
}

Double MVBaseline::
operator*(const MVBaseline &other) const {
  Double tmp = 0.0;
  for (Int i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

MVBaseline MVBaseline::operator-() const {
  MVBaseline tmp; tmp = *this;
  tmp.xyz.ac() = -xyz.ac();
  return tmp;
}

MVBaseline &MVBaseline::operator+=(const MVBaseline &right) {
  xyz.ac() += right.xyz.ac();
  return *this;
}

MVBaseline MVBaseline::operator+(const MVBaseline &right) const {
  MVBaseline tmp = *this;
  tmp += right;
  return tmp;
}

MVBaseline &MVBaseline::operator-=(const MVBaseline &right) {
  xyz.ac() -= right.xyz.ac();
  return *this;
}

MVBaseline MVBaseline::operator-(const MVBaseline &right) const{
  MVBaseline tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

uInt MVBaseline::type() const {
  return Register((MVBaseline *)0);
}

void MVBaseline::assert(const MeasValue &in) {
  if (in.type() != Register((MVBaseline *)0)) {
    throw(AipsError("Illegal MeasValue type argument: MVBaseline"));
  };
}

void MVBaseline::adjust() {}

void MVBaseline::adjust(Double &res) {
  res = sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz.ac() /= res;
  };
}

void MVBaseline::readjust(Double res) {
  if (res == 0.0) {
    xyz.ac() *= 1e-12;
  } else {
    xyz.ac() *= res;
  };
}

Double MVBaseline::radius() {
  return (sqrt(operator*(*this)));
}

Vector<Double> MVBaseline::get() const{
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

const Vector<Double> &MVBaseline::getValue() const {
  return xyz;
}

Quantum<Vector<Double> > MVBaseline::getAngle() const{
  Vector<Double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<Double> >(tmp,"rad");
}

Quantum<Vector<Double> > MVBaseline::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVBaseline::getLength() const{
  Double tmp = sqrt(operator*(*this));
  return Quantity(tmp,"m");
}

Quantity MVBaseline::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVBaseline::BaselineAngle(const MVBaseline &other) const {
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

Quantity MVBaseline::BaselineAngle(const MVBaseline &other, 
				   const Unit &unit) const {
  return Quantity(BaselineAngle(other), "rad").get(unit);
}

Double MVBaseline::separation(const MVBaseline &other) const {
  MVBaseline t1(*this);
  MVBaseline t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  Double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*asin(d1));
}

Quantity MVBaseline::separation(const MVBaseline &other, 
				const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

MVBaseline MVBaseline::crossProduct(const MVBaseline &other) const {
  MVBaseline res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

void MVBaseline::print(ostream &os) const {
  os << getValue().ac();
}

MeasValue *MVBaseline::clone() const {
  return (new MVBaseline(*this));
}

Vector<Double> MVBaseline::getVector() const {
  return xyz;
}

void MVBaseline::putVector(const Vector<Double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uInt i=0; i<in.nelements();i++) xyz(i) = in(i);
  };
}

Vector<Quantum<Double> > MVBaseline::getRecordValue() const {
  Vector<Double> t(3);
  t = get();
  Vector<Quantum<Double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<Double> > MVBaseline::getXRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

Bool MVBaseline::putValue(const Vector<Quantum<Double> > &in) {
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

MVBaseline operator*(const RotMatrix &left, const MVBaseline &right) {
  MVBaseline result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVBaseline operator*(const MVBaseline &left, const RotMatrix &right) {
  MVBaseline result(left);
  result *= right;
  return result;
}

MVBaseline operator*(Double left, const MVBaseline &right) {
  MVBaseline result(right);
  result *= left;
  return result;
}

MVBaseline operator*(const MVBaseline &left, Double right) {
  MVBaseline result(left);
  result *= right;
  return result;
}

Double operator*(const Vector<Double> &left, const MVBaseline &right) {
  MVBaseline tmp(left);
  return (tmp * right);
}

Double operator*(const MVBaseline &left, const Vector<Double> &right) {
  MVBaseline tmp(right);
  return (tmp * left);
}

Double operator*(const MVPosition &left, const MVBaseline &right) {
  MVBaseline tmp(left);
  return (tmp * right);
}

Double operator*(const MVBaseline &left, const MVPosition &right) {
  MVBaseline tmp(right);
  return (tmp * left);
}
