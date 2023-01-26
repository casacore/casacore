//# MVEarthMagnetic.cc: A 3D Earth magnetic field vector
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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
#include <casacore/casa/Quanta/MVEarthMagnetic.h>
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

// MVEarthMagnetic class

//# Constructors
MVEarthMagnetic::MVEarthMagnetic() :
  MVPosition() {}

MVEarthMagnetic::MVEarthMagnetic(const MVPosition &other) : 
  MVPosition(other) {}

MVEarthMagnetic::MVEarthMagnetic(double in) :
  MVPosition(in) {}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l) :
  MVPosition() {
    static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    xyz(2) = l.getBaseValue();
  }

MVEarthMagnetic::MVEarthMagnetic(double in0, double in1, double in2) : 
  MVPosition(in0, in1, in2) {}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, double angle0, double angle1) : 
  MVPosition() {
    static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    double loc = std::cos(angle1);
    xyz(0) = std::cos(angle0)*loc;
    xyz(1) = std::sin(angle0)*loc;
    xyz(2) = std::sin(angle1);
    readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, const Quantity &angle0, 
				 const Quantity &angle1) : 
  MVPosition() {
    static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    double loc = (cos(angle1)).getValue();
    xyz(0) = ((cos(angle0)).getValue()) * loc;
    xyz(1) = ((sin(angle0)).getValue()) * loc;
    xyz(2) = (sin(angle1)).getValue();
    readjust(l.getBaseValue());
  }

MVEarthMagnetic::MVEarthMagnetic(const Quantum<Vector<double> > &angle) :
  MVPosition() {
    static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uint32_t i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
    } else if (i == 3) {
      angle.assure(testUnit);
      xyz = angle.getBaseValue();
    } else {
      Vector<double> tsin = (sin(angle)).getValue(); 
      Vector<double> tcos = (cos(angle)).getValue(); 
      xyz = double(0.0);
      if (i > 1) {
	xyz(0) = tcos(0) * tcos(1);
	xyz(1) = tsin(0) * tcos(1);
	xyz(2) = tsin(1);
      } else if (i > 0) {
	xyz(0) = tcos(0);
	xyz(1) = tsin(0);
      } else {
	xyz(2)=1.0;
      }
    }
  }

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, 
		     const Quantum<Vector<double> > &angle) :
  MVPosition() {
    static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uint32_t i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
    } else if (i == 3) {
      angle.assure(UnitVal::NODIM);
      xyz = angle.getValue();
    } else {
      Vector<double> tsin = (sin(angle)).getValue(); 
      Vector<double> tcos = (cos(angle)).getValue(); 
      xyz = double(0.0);
      if (i > 1) {
	xyz(0) = tcos(0) * tcos(1);
	xyz(1) = tsin(0) * tcos(1);
	xyz(2) = tsin(1);
      } else if (i > 0) {
	xyz(0) = tcos(0);
	xyz(1) = tsin(0);
      } else {
	xyz(2)=1.0;
      }
    }
    l.assure(testUnit);
    readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Vector<double> &other) :
  MVPosition(other) {}

MVEarthMagnetic::MVEarthMagnetic(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantity vector in MVEarthMagnetic constructor"));
    }
  }

//# Operators
bool MVEarthMagnetic::
operator==(const MVEarthMagnetic &other) const {
  return (allEQ(xyz, other.xyz));
}

bool MVEarthMagnetic::
operator!=(const MVEarthMagnetic &other) const {
  return (!(*this == other));
}

bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

bool MVEarthMagnetic::
nearAbs(const MVEarthMagnetic &other, double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

double MVEarthMagnetic::
operator*(const MVEarthMagnetic &other) const {
  double tmp = 0.0;
  for (int32_t i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

MVEarthMagnetic MVEarthMagnetic::operator-() const {
  MVEarthMagnetic tmp; tmp = *this;
  tmp.xyz = -xyz;
  return tmp;
}

MVEarthMagnetic &MVEarthMagnetic::operator+=(const MVEarthMagnetic &right) {
  xyz += right.xyz;
  return *this;
}

MVEarthMagnetic MVEarthMagnetic::operator+(const MVEarthMagnetic &right) const {
  MVEarthMagnetic tmp = *this;
  tmp += right;
  return tmp;
}

MVEarthMagnetic &MVEarthMagnetic::operator-=(const MVEarthMagnetic &right) {
  xyz -= right.xyz;
  return *this;
}

MVEarthMagnetic MVEarthMagnetic::operator-(const MVEarthMagnetic &right) const{
  MVEarthMagnetic tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

void MVEarthMagnetic::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVEarthMagnetic*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVEarthMagnetic"));
  }
}

void MVEarthMagnetic::adjust() {}

void MVEarthMagnetic::adjust(double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVEarthMagnetic::readjust(double res) {
  if (res == 0.0) {
    xyz *= 1e-6;
  } else {
    xyz *= res;
  }
}

double MVEarthMagnetic::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<double> MVEarthMagnetic::get() const{
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

const Vector<double> &MVEarthMagnetic::getValue() const {
  return xyz;
}

Quantum<Vector<double> > MVEarthMagnetic::getAngle() const{
  Vector<double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<double> >(tmp,"rad");
}

Quantum<Vector<double> > MVEarthMagnetic::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVEarthMagnetic::getLength() const{
  double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp,"nT");
}

Quantity MVEarthMagnetic::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

double MVEarthMagnetic::earthMagneticAngle(const MVEarthMagnetic &other) const {
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

Quantity MVEarthMagnetic::earthMagneticAngle(const MVEarthMagnetic &other, 
					     const Unit &unit) const {
  return Quantity(earthMagneticAngle(other), "rad").get(unit);
}

double MVEarthMagnetic::separation(const MVEarthMagnetic &other) const {
  MVEarthMagnetic t1(*this);
  MVEarthMagnetic t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*std::asin(d1));
}

Quantity MVEarthMagnetic::separation(const MVEarthMagnetic &other, 
				     const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

MVEarthMagnetic MVEarthMagnetic::crossProduct(const MVEarthMagnetic &other) const {
  MVEarthMagnetic res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

void MVEarthMagnetic::print(ostream &os) const {
  os << getValue();
}

MeasValue *MVEarthMagnetic::clone() const {
  return (new MVEarthMagnetic(*this));
}

Vector<double> MVEarthMagnetic::getVector() const {
  return xyz;
}

void MVEarthMagnetic::putVector(const Vector<double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uint32_t i=0; i<in.nelements(); i++) xyz(i) = in(i);
  }
}

Vector<Quantum<double> > MVEarthMagnetic::getRecordValue() const {
  Vector<Quantum<double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "nT");
  tmp(1) = Quantity(xyz(1), "nT");
  tmp(2) = Quantity(xyz(2), "nT");
  return tmp;
}

bool MVEarthMagnetic::putValue(const Vector<Quantum<double> > &in) {
  static const UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
    UnitVal::CURRENT;
  uint32_t i; i = in.nelements();
  if (i != 3 ) return false;
  if (in(0).check(testUnit)) {
    if (in(1).check(testUnit) &&
	in(2).check(testUnit)) {
      for (uint32_t j = 0; j<i; j++) {
	xyz(j) = in(j).get("nT").getValue();
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
  } else if (in(2).check(testUnit)) {
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
  } else {
    return false;
  }
  return true;
}

MVEarthMagnetic operator*(const RotMatrix &left,
			  const MVEarthMagnetic &right) {
  MVEarthMagnetic result;
  for (int32_t i=0; i<3; i++) {
    result(i) = 0;
    for (int32_t j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVEarthMagnetic operator*(const MVEarthMagnetic &left,
			  const RotMatrix &right) {
  MVEarthMagnetic result(left);
  result *= right;
  return result;
}

MVEarthMagnetic operator*(double left, const MVEarthMagnetic &right) {
  MVEarthMagnetic result(right);
  result *= left;
  return result;
}

MVEarthMagnetic operator*(const MVEarthMagnetic &left, double right) {
  MVEarthMagnetic result(left);
  result *= right;
  return result;
}

double operator*(const Vector<double> &left, const MVEarthMagnetic &right) {
  MVEarthMagnetic tmp(left);
  return (tmp * right);
}

double operator*(const MVEarthMagnetic &left, const Vector<double> &right) {
  MVEarthMagnetic tmp(right);
  return (tmp * left);
}

double operator*(const MVPosition &left, const MVEarthMagnetic &right) {
  MVEarthMagnetic tmp(left);
  return (tmp * right);
}

double operator*(const MVEarthMagnetic &left, const MVPosition &right) {
  MVEarthMagnetic tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

