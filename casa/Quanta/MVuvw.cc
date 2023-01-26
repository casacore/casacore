//# MVuvw.cc: A 3D vector on Earth
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
#include <casacore/casa/Quanta/MVuvw.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVBaseline.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVuvw class

//# Constructors
MVuvw::MVuvw() :
  MVPosition() {}

MVuvw::MVuvw(double in) :
  MVPosition(in) {}

MVuvw::MVuvw(const Quantity &l) :
  MVPosition(l) {}

MVuvw::MVuvw(double in0, double in1, double in2) : 
  MVPosition(in0, in1, in2) {}

MVuvw::MVuvw(const Quantity &l, double angle0, double angle1) : 
  MVPosition(l, angle0, angle1) {}

MVuvw::MVuvw(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  MVPosition(l, angle0, angle1) {}

MVuvw::MVuvw(const Quantum<Vector<double> > &angle) :
  MVPosition(angle) {}

MVuvw::MVuvw(const Quantity &l, 
		       const Quantum<Vector<double> > &angle) :
  MVPosition(l, angle) {}

MVuvw::MVuvw(const Vector<double> &other) :
  MVPosition(other) {}

MVuvw::MVuvw(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantum vector in MVuvw constructor"));
    }
  }

MVuvw::MVuvw(const MVBaseline &pos, const MVDirection &dr, bool ew) :
  MVPosition() {
  // Next for sgi_ntv to get it working properly
  MVDirection dr1(dr);
  dr1.adjust();
  RotMatrix x(Euler(dr1.getLat() - C::pi_2, 1u,
		    -dr1.getLong() - C::pi_2, 3u));
  if (ew) {}
  xyz = (x * pos).getValue();
}

MVuvw::MVuvw(const MVPosition &other) :
  MVPosition(other) {}

//# Operators
bool MVuvw::
operator==(const MVuvw &other) const {
  return (allEQ(xyz, other.xyz));
}

bool MVuvw::
operator!=(const MVuvw &other) const {
  return (!(*this == other));
}

bool MVuvw::
near(const MVuvw &other, double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

bool MVuvw::
near(const MVuvw &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

bool MVuvw::
nearAbs(const MVuvw &other, double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

double MVuvw::
operator*(const MVuvw &other) const {
  double tmp = 0.0;
  for (int32_t i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

MVuvw MVuvw::operator-() const {
  MVuvw tmp; tmp = *this;
  tmp.xyz = -xyz;
  return tmp;
}

MVuvw &MVuvw::operator+=(const MVuvw &right) {
  xyz += right.xyz;
  return *this;
}

MVuvw MVuvw::operator+(const MVuvw &right) const {
  MVuvw tmp = *this;
  tmp += right;
  return tmp;
}

MVuvw &MVuvw::operator-=(const MVuvw &right) {
  xyz -= right.xyz;
  return *this;
}

MVuvw MVuvw::operator-(const MVuvw &right) const{
  MVuvw tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

void MVuvw::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVuvw*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVuvw"));
  }
}

void MVuvw::adjust() {}

void MVuvw::adjust(double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVuvw::readjust(double res) {
  if (res == 0.0) {
    xyz *= 1e-12;
  } else {
    xyz *= res;
  }
}

double MVuvw::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<double> MVuvw::get() const{
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

const Vector<double> &MVuvw::getValue() const {
  return xyz;
}

Quantum<Vector<double> > MVuvw::getAngle() const{
  Vector<double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<double> >(tmp,"rad");
}

Quantum<Vector<double> > MVuvw::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVuvw::getLength() const{
  double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp,"m");
}

Quantity MVuvw::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

double MVuvw::uvwAngle(const MVuvw &other) const {
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

Quantity MVuvw::uvwAngle(const MVuvw &other, 
				   const Unit &unit) const {
  return Quantity(uvwAngle(other), "rad").get(unit);
}

double MVuvw::separation(const MVuvw &other) const {
  MVuvw t1(*this);
  MVuvw t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*std::asin(d1));
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
  os << getValue();
}

MeasValue *MVuvw::clone() const {
  return (new MVuvw(*this));
}

Vector<double> MVuvw::getVector() const {
  return xyz;
}

void MVuvw::putVector(const Vector<double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uint32_t i=0; i<in.nelements();i++) xyz(i) = in(i);
  }
}

Vector<Quantum<double> > MVuvw::getRecordValue() const {
  Vector<double> t(3);
  t = get();
  Vector<Quantum<double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<double> > MVuvw::getXRecordValue() const {
  Vector<Quantum<double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

bool MVuvw::putValue(const Vector<Quantum<double> > &in) {
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

MVuvw operator*(const RotMatrix &left, const MVuvw &right) {
  MVuvw result;
  for (int32_t i=0; i<3; i++) {
    result(i) = 0;
    for (int32_t j=0; j<3; j++) {
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

MVuvw operator*(double left, const MVuvw &right) {
  MVuvw result(right);
  result *= left;
  return result;
}

MVuvw operator*(const MVuvw &left, double right) {
  MVuvw result(left);
  result *= right;
  return result;
}

double operator*(const Vector<double> &left, const MVuvw &right) {
  MVuvw tmp(left);
  return (tmp * right);
}

double operator*(const MVuvw &left, const Vector<double> &right) {
  MVuvw tmp(right);
  return (tmp * left);
}

double operator*(const MVPosition &left, const MVuvw &right) {
  MVuvw tmp(left);
  return (tmp * right);
}

double operator*(const MVuvw &left, const MVPosition &right) {
  MVuvw tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

