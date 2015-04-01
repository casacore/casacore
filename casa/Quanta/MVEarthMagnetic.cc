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
//#
//# $Id$

//# Includes
#include <casacore/casa/Quanta/MVEarthMagnetic.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVEarthMagnetic class

//# Constructors
MVEarthMagnetic::MVEarthMagnetic() :
  MVPosition() {}

MVEarthMagnetic::MVEarthMagnetic(const MVPosition &other) : 
  MVPosition(other) {}

MVEarthMagnetic &MVEarthMagnetic::operator=(const MVEarthMagnetic &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVEarthMagnetic::MVEarthMagnetic(Double in) :
  MVPosition(in) {}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l) :
  MVPosition() {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    xyz(2) = l.getBaseValue();
  }

MVEarthMagnetic::MVEarthMagnetic(Double in0, Double in1, Double in2) : 
  MVPosition(in0, in1, in2) {}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, Double angle0, Double angle1) : 
  MVPosition() {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    Double loc = std::cos(angle1);
    xyz(0) = std::cos(angle0)*loc;
    xyz(1) = std::sin(angle0)*loc;
    xyz(2) = std::sin(angle1);
    readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, const Quantity &angle0, 
				 const Quantity &angle1) : 
  MVPosition() {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assure(testUnit);
    Double loc = (cos(angle1)).getValue();
    xyz(0) = ((cos(angle0)).getValue()) * loc;
    xyz(1) = ((sin(angle0)).getValue()) * loc;
    xyz(2) = (sin(angle1)).getValue();
    readjust(l.getBaseValue());
  }

MVEarthMagnetic::MVEarthMagnetic(const Quantum<Vector<Double> > &angle) :
  MVPosition() {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
    } else if (i == 3) {
      angle.assure(testUnit);
      xyz = angle.getBaseValue();
    } else {
      Vector<Double> tsin = (sin(angle)).getValue(); 
      Vector<Double> tcos = (cos(angle)).getValue(); 
      xyz = Double(0.0);
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
		     const Quantum<Vector<Double> > &angle) :
  MVPosition() {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
    } else if (i == 3) {
      angle.assure(UnitVal::NODIM);
      xyz = angle.getValue();
    } else {
      Vector<Double> tsin = (sin(angle)).getValue(); 
      Vector<Double> tcos = (cos(angle)).getValue(); 
      xyz = Double(0.0);
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

MVEarthMagnetic::MVEarthMagnetic(const Vector<Double> &other) :
  MVPosition(other) {}

MVEarthMagnetic::MVEarthMagnetic(const Vector<Quantity> &other) :
  MVPosition() {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantity vector in MVEarthMagnetic constructor"));
    }
  }

//# Destructor
MVEarthMagnetic::~MVEarthMagnetic() {}

//# Operators
Bool MVEarthMagnetic::
operator==(const MVEarthMagnetic &other) const {
  return (allEQ(xyz, other.xyz));
}

Bool MVEarthMagnetic::
operator!=(const MVEarthMagnetic &other) const {
  return (!(*this == other));
}

Bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, Double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

Bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVEarthMagnetic::
nearAbs(const MVEarthMagnetic &other, Double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

Double MVEarthMagnetic::
operator*(const MVEarthMagnetic &other) const {
  Double tmp = 0.0;
  for (Int i=0; i<3; i++) {
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

uInt MVEarthMagnetic::type() const {
  return Register(static_cast<MVEarthMagnetic *>(0));
}

void MVEarthMagnetic::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVEarthMagnetic *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVEarthMagnetic"));
  }
}

void MVEarthMagnetic::adjust() {}

void MVEarthMagnetic::adjust(Double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVEarthMagnetic::readjust(Double res) {
  if (res == 0.0) {
    xyz *= 1e-6;
  } else {
    xyz *= res;
  }
}

Double MVEarthMagnetic::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<Double> MVEarthMagnetic::get() const{
  Vector<Double> tmp(3);
  tmp(0) = std::sqrt(operator*(*this));
  Double ln = (tmp(0) == 0.0 ? 1.0 : tmp(0));
  Double loc = xyz(0)/ln;
  if (loc == 0) {
    tmp(1) = std::asin(xyz(1)/ln);
  } else {
    tmp(1) = std::atan2(xyz(1),xyz(0));
  }
  tmp(2) = std::asin(xyz(2)/ln);
  return tmp;
}

const Vector<Double> &MVEarthMagnetic::getValue() const {
  return xyz;
}

Quantum<Vector<Double> > MVEarthMagnetic::getAngle() const{
  Vector<Double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<Double> >(tmp,"rad");
}

Quantum<Vector<Double> > MVEarthMagnetic::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Quantity MVEarthMagnetic::getLength() const{
  Double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp,"nT");
}

Quantity MVEarthMagnetic::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVEarthMagnetic::earthMagneticAngle(const MVEarthMagnetic &other) const {
  Vector<Double> t1(3);
  Vector<Double> t2(3);
  t1 = get();
  t2 = other.get();
  Double s1,c1;
  c1 = std::cos(t1(2)) * std::sin(t2(2)) -
    std::sin(t1(2)) * std::cos(t2(2)) * std::cos(t1(1) - t2(1));
  s1 = -std::cos(t2(2)) * std::sin(t1(1) - t2(1));
  if (s1 != 0 || c1 != 0) {
    return std::atan2(s1, c1);
  } else {
    return Double(0.0);
  }
}

Quantity MVEarthMagnetic::earthMagneticAngle(const MVEarthMagnetic &other, 
					     const Unit &unit) const {
  return Quantity(earthMagneticAngle(other), "rad").get(unit);
}

Double MVEarthMagnetic::separation(const MVEarthMagnetic &other) const {
  MVEarthMagnetic t1(*this);
  MVEarthMagnetic t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  Double d1 = t1.radius()/2.0;
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

Vector<Double> MVEarthMagnetic::getVector() const {
  return xyz;
}

void MVEarthMagnetic::putVector(const Vector<Double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uInt i=0; i<in.nelements(); i++) xyz(i) = in(i);
  }
}

Vector<Quantum<Double> > MVEarthMagnetic::getRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "nT");
  tmp(1) = Quantity(xyz(1), "nT");
  tmp(2) = Quantity(xyz(2), "nT");
  return tmp;
}

Bool MVEarthMagnetic::putValue(const Vector<Quantum<Double> > &in) {
  static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
    UnitVal::CURRENT;
  uInt i; i = in.nelements();
  if (i != 3 ) return False;
  if (in(0).check(testUnit)) {
    if (in(1).check(testUnit) &&
	in(2).check(testUnit)) {
      for (uInt j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      }
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      for (uInt j=1; j < i; j++) {
	tsin(j-1) = (sin(in(j))).getValue(); 
	tcos(j-1) = (cos(in(j))).getValue(); 
      }
      xyz = Double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(0).getBaseValue());
    } else {
      return False;
    }
  } else if (in(2).check(testUnit)) {
    if (in(0).check(UnitVal::ANGLE) &&
	in(1).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      Int j;
      for (j=0; j < 2; j++) {
	tsin(j) = (sin(in(j))).getValue(); 
	tcos(j) = (cos(in(j))).getValue(); 
      }
      xyz = Double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      readjust(in(2).getBaseValue());
    } else {
      return False;
    }
  } else {
    return False;
  }
  return True;
}

MVEarthMagnetic operator*(const RotMatrix &left,
			  const MVEarthMagnetic &right) {
  MVEarthMagnetic result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
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

MVEarthMagnetic operator*(Double left, const MVEarthMagnetic &right) {
  MVEarthMagnetic result(right);
  result *= left;
  return result;
}

MVEarthMagnetic operator*(const MVEarthMagnetic &left, Double right) {
  MVEarthMagnetic result(left);
  result *= right;
  return result;
}

Double operator*(const Vector<Double> &left, const MVEarthMagnetic &right) {
  MVEarthMagnetic tmp(left);
  return (tmp * right);
}

Double operator*(const MVEarthMagnetic &left, const Vector<Double> &right) {
  MVEarthMagnetic tmp(right);
  return (tmp * left);
}

Double operator*(const MVPosition &left, const MVEarthMagnetic &right) {
  MVEarthMagnetic tmp(left);
  return (tmp * right);
}

Double operator*(const MVEarthMagnetic &left, const MVPosition &right) {
  MVEarthMagnetic tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

