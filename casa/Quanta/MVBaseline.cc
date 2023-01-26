//# MVBaseline.cc: A 3D vector on Earth
//# Copyright (C) 1998,1999,2000,2001
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

//# Includes
#include <casacore/casa/Quanta/MVBaseline.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVBaseline class

//# Constructors
MVBaseline::MVBaseline() :
  MVPosition() {}

MVBaseline::MVBaseline(double in) :
  MVPosition(in) {}

MVBaseline::MVBaseline(const Quantity &l) :
  MVPosition(l) {}

MVBaseline::MVBaseline(double in0, double in1, double in2) : 
  MVPosition(in0, in1, in2) {}

MVBaseline::MVBaseline(const Quantity &l, double angle0, double angle1) : 
  MVPosition(l, angle0, angle1) {}

MVBaseline::MVBaseline(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  MVPosition(l, angle0, angle1) {}

MVBaseline::MVBaseline(const Quantum<Vector<double> > &angle) :
  MVPosition(angle) {}

MVBaseline::MVBaseline(const Quantity &l, 
		       const Quantum<Vector<double> > &angle) :
  MVPosition(l, angle) {}

MVBaseline::MVBaseline(const Vector<double> &other) :
  MVPosition(other) {}

MVBaseline::MVBaseline(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantum vector in MVBaseline constructor"));
    }
  }

MVBaseline::MVBaseline(const MVPosition &pos, const MVPosition &base) :
  MVPosition(pos) {
    xyz -= base.getValue();
}

MVBaseline::MVBaseline(const MVPosition &other) :
  MVPosition(other) {}

//# Operators
bool MVBaseline::
operator==(const MVBaseline &other) const {
  return (allEQ(xyz, other.xyz));
}

bool MVBaseline::
operator!=(const MVBaseline &other) const {
  return (!(*this == other));
}

bool MVBaseline::
near(const MVBaseline &other, double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

bool MVBaseline::
near(const MVBaseline &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

bool MVBaseline::
nearAbs(const MVBaseline &other, double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

double MVBaseline::
operator*(const MVBaseline &other) const {
  double tmp = 0.0;
  for (int32_t i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

MVBaseline MVBaseline::operator-() const {
  MVBaseline tmp; tmp = *this;
  tmp.xyz = -xyz;
  return tmp;
}

MVBaseline &MVBaseline::operator+=(const MVBaseline &right) {
  xyz += right.xyz;
  return *this;
}

MVBaseline MVBaseline::operator+(const MVBaseline &right) const {
  MVBaseline tmp = *this;
  tmp += right;
  return tmp;
}

MVBaseline &MVBaseline::operator-=(const MVBaseline &right) {
  xyz -= right.xyz;
  return *this;
}

MVBaseline MVBaseline::operator-(const MVBaseline &right) const{
  MVBaseline tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

void MVBaseline::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVBaseline*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVBaseline"));
  }
}

void MVBaseline::adjust() {}

void MVBaseline::adjust(double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVBaseline::readjust(double res) {
  if (res == 0.0) {
    xyz *= 1e-12;
  } else {
    xyz *= res;
  }
}

double MVBaseline::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<double> MVBaseline::get() const{
  Vector<double> tmp(3);
  tmp(0) = std::sqrt(operator*(*this));
  double ln = (tmp(0) == 0.0 ? 1.0 : tmp(0));
  double loc = xyz(0)/ln;
  if (loc == 0) {
    tmp(1) = std::asin(xyz(1)/ln);
  } else {
    tmp(1) = std::atan2(xyz(1),xyz(0));
  }
  tmp(2) = std::asin(xyz(2)/ln);
  return tmp;
}

const Vector<double> &MVBaseline::getValue() const {
  return xyz;
}

Quantum<Vector<double> > MVBaseline::getAngle() const{
  Vector<double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<double> >(tmp,"rad");
}

Quantum<Vector<double> > MVBaseline::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVBaseline::getLength() const{
  double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp,"m");
}

Quantity MVBaseline::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

double MVBaseline::BaselineAngle(const MVBaseline &other) const {
  Vector<double> t1(3);
  Vector<double> t2(3);
  t1 = get();
  t2 = other.get();
  double s1,c1;
  c1 = std::cos(t1(2)) * std::sin(t2(2)) -
    std::sin(t1(2)) * std::cos(t2(2)) * std::cos(t1(1) - t2(1));
  s1 = -std::cos(t2(2)) * std::sin(t1(1) - t2(1));
  if (s1 != 0 || c1 != 0) {
    return std::atan2(s1, c1);
  } else {
    return double(0.0);
  }
}

Quantity MVBaseline::BaselineAngle(const MVBaseline &other, 
				   const Unit &unit) const {
  return Quantity(BaselineAngle(other), "rad").get(unit);
}

double MVBaseline::separation(const MVBaseline &other) const {
  MVBaseline t1(*this);
  MVBaseline t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*std::asin(d1));
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
  os << getValue();
}

MeasValue *MVBaseline::clone() const {
  return (new MVBaseline(*this));
}

Vector<double> MVBaseline::getVector() const {
  return xyz;
}

void MVBaseline::putVector(const Vector<double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uint32_t i=0; i<in.nelements();i++) xyz(i) = in(i);
  }
}

Vector<Quantum<double> > MVBaseline::getRecordValue() const {
  Vector<double> t(3);
  t = get();
  Vector<Quantum<double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<double> > MVBaseline::getXRecordValue() const {
  Vector<Quantum<double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

bool MVBaseline::putValue(const Vector<Quantum<double> > &in) {
  uint32_t i = in.nelements();
  if (i != 3 ) return false;
  if (in(0).check(UnitVal::LENGTH)) {
    if (in(1).check(UnitVal::LENGTH) &&
	in(2).check(UnitVal::LENGTH)) {
      for (uint32_t j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      }
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<double> tsin(2), tcos(2);
      for (uint32_t j=1; j < i; j++) {
	tsin(j-1) = (sin(in(j))).getValue(); 
	tcos(j-1) = (cos(in(j))).getValue(); 
      }
      xyz = double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(0).getBaseValue());
    } else {
      return false;
    }
  } else if (in(2).check(UnitVal::LENGTH)) {
    if (in(0).check(UnitVal::ANGLE) &&
	in(1).check(UnitVal::ANGLE)) {
      Vector<double> tsin(2), tcos(2);
      int32_t j;
      for (j=0; j < 2; j++) {
	tsin(j) = (sin(in(j))).getValue(); 
	tcos(j) = (cos(in(j))).getValue(); 
      }
      xyz = double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(2).getBaseValue());
    } else {
      return false;
    }
  }
  return true;
}

MVBaseline operator*(const RotMatrix &left, const MVBaseline &right) {
  MVBaseline result;
  for (int32_t i=0; i<3; i++) {
    result(i) = 0;
    for (int32_t j=0; j<3; j++) {
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

MVBaseline operator*(double left, const MVBaseline &right) {
  MVBaseline result(right);
  result *= left;
  return result;
}

MVBaseline operator*(const MVBaseline &left, double right) {
  MVBaseline result(left);
  result *= right;
  return result;
}

double operator*(const Vector<double> &left, const MVBaseline &right) {
  MVBaseline tmp(left);
  return (tmp * right);
}

double operator*(const MVBaseline &left, const Vector<double> &right) {
  MVBaseline tmp(right);
  return (tmp * left);
}

double operator*(const MVPosition &left, const MVBaseline &right) {
  MVBaseline tmp(left);
  return (tmp * right);
}

double operator*(const MVBaseline &left, const MVPosition &right) {
  MVBaseline tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

