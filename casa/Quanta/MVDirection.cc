//# MVDirection.cc: Vector of three direction cosines
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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVDirection class

//# Constructors
MVDirection::MVDirection() :
  MVPosition() {
    xyz(2) = Double(1.0);
}

MVDirection::MVDirection(const MVPosition &other) : 
  MVPosition(other) {}

MVDirection &MVDirection::operator=(const MVDirection &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVDirection::MVDirection(Double in0, Double in1, Double in2) : 
  MVPosition(in0,in1,in2) {
    adjust();
  }

MVDirection::MVDirection(Double in0) :
  MVPosition() {
    xyz(0) = std::cos(in0);
    xyz(1) = std::sin(in0);
  }

MVDirection::MVDirection(Double angle0, Double angle1) : 
  MVPosition() {
    Double loc = std::cos(angle1);
    xyz(0) = std::cos(angle0)*loc;
    xyz(1) = std::sin(angle0)*loc;
    xyz(2) = std::sin(angle1);
  }

MVDirection::MVDirection(const Quantity &angle0) :
  MVPosition() {
    xyz(0) = ((cos(angle0)).getValue());
    xyz(1) = ((sin(angle0)).getValue());
    xyz(2) = 0;
  }

MVDirection::MVDirection(const Quantity &angle0, const Quantity &angle1) : 
  MVPosition() {
    Double loc = (cos(angle1)).getValue();
    xyz(0) = ((cos(angle0)).getValue()) * loc;
    xyz(1) = ((sin(angle0)).getValue()) * loc;
    xyz(2) = (sin(angle1)).getValue();
  }

MVDirection::MVDirection(const Quantum<Vector<Double> > &angle) :
  MVPosition(angle) {
    adjust();
  }

MVDirection::MVDirection(const Vector<Double> &angle) :
  MVPosition(angle) {
    adjust();
  }

MVDirection::MVDirection(const Vector<Quantity> &angle) :
  MVPosition() {
    if (!putValue(angle)) {
      throw (AipsError("Illegal quantum vector in MVDirection constructor"));
    }
  }

//# Destructor
MVDirection::~MVDirection() {}

//# Operators
MVDirection &MVDirection::operator+=(const MVDirection &right) {
  xyz += right.xyz;
  adjust();
  return *this;
}

MVDirection MVDirection::operator+(const MVDirection &right) const{
  MVDirection tmp = *this;
  tmp += right;
  return tmp;
}

MVDirection &MVDirection::operator-=(const MVDirection &right) {
  xyz -= right.xyz;
  adjust();
  return *this;
}

MVDirection MVDirection::operator-(const MVDirection &right) const{
  MVDirection tmp = *this;
  tmp -= right;
  return tmp;
}

//# Member functions

uInt MVDirection::type() const {
  return Register(static_cast<MVDirection *>(0));
}

void MVDirection::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVDirection *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVDirection"));
  }
}

void MVDirection::adjust() {
  Double length = std::sqrt(operator*(*this));
  if (length == 0) {
    xyz(2) = 1.0;
  } else if (length != 1.0) {
    xyz /= length;
  }
}

void MVDirection::adjust(Double &res) {
  res = std::sqrt(operator*(*this));
  if (res == 0) {
    xyz(2) = 1.0;
  } else if (res != 1.0) {
    xyz /= res;
  }
}

Vector<Double> MVDirection::get() const {
  Vector<Double> tmp(2);
  if (xyz(0) != 0 || xyz(1) != 0) tmp(0) = std::atan2(xyz(1),xyz(0));
  else tmp(0) = 0.0;
  tmp(1) = std::asin(xyz(2));
  return tmp;
}    

Double MVDirection::getLat() const {
  return MVPosition::getLat(1.0);
}

Quantity MVDirection::getLat(const Unit &unit) const {
  return (Quantity(getLat(), "rad").get(unit));
}

Vector<Quantum<Double> > MVDirection::getRecordValue() const {
  Vector<Double> t(2);
  t = get();
  Vector<Quantum<Double> > tmp(2);
  tmp(0) = Quantity(t(0), "rad"); 
  tmp(1) = Quantity(t(1), "rad"); 
  return tmp;
}

Vector<Quantum<Double> > MVDirection::getXRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "");
  tmp(1) = Quantity(xyz(1), "");
  tmp(2) = Quantity(xyz(2), "");
  return tmp;
}

Vector<Quantum<Double> > MVDirection::getTMRecordValue() const {
  return getRecordValue();
}

Bool MVDirection::putValue(const Vector<Quantum<Double> > &in) {
  uInt i; i = in.nelements();
  if (i > 3 ) return False;
  if (i == 3 &&
      in(0).check(UnitVal::NODIM) &&
      in(1).check(UnitVal::NODIM) &&
      in(2).check(UnitVal::NODIM)) {
    for (uInt j = 0; j<i; j++) {
      xyz(j) = in(j).getValue();
    }
    adjust();
  } else {
    uInt j;
    for (j = 0; j<i; j++) {
      if (!in(j).check(UnitVal::ANGLE)) return False;
    }
    Vector<Double> tsin(i), tcos(i);
    for (j=0; j < i; j++) {
      tsin(j) = (sin(in(j))).getValue(); 
      tcos(j) = (cos(in(j))).getValue(); 
    }
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
    adjust();
  }
  return True;
}

void MVDirection::setAngle(Double angle0, Double angle1) { 
  Double loc = std::cos(angle1);
  xyz(0) = std::cos(angle0)*loc;
  xyz(1) = std::sin(angle0)*loc;
  xyz(2) = std::sin(angle1);
}

Double MVDirection::positionAngle(const MVPosition &other) const {
  Double longDiff(getLong() - other.getLong());
  Double slat1(xyz(2));
  Double ln(norm(other.getValue())); 
  Double slat2(other.getValue()(2)/ln);
  Double clat2(std::sqrt(std::fabs(1.0 - slat2*slat2)));
  Double s1(-clat2 * std::sin(longDiff));
  Double c1(std::sqrt(std::fabs(1.0 - slat1*slat1))*slat2 - slat1*clat2*std::cos(longDiff));
  return ((s1 != 0 || c1 != 0) ? std::atan2(s1, c1): 0.0);
}

Double MVDirection::positionAngle(const MVDirection &other) const {
  const Double longDiff(getLong() - other.getLong());
  const Double slat1(xyz(2));
  const Double slat2(other.xyz(2));
  const Double clat2(std::sqrt(std::fabs(1.0 - slat2*slat2)));
  const Double s1(-clat2 * std::sin(longDiff));
  const Double c1(std::sqrt(std::fabs(1.0 - slat1*slat1))*slat2 -
		  slat1*clat2*std::cos(longDiff));
  return ((s1 != 0 || c1 != 0) ? std::atan2(s1, c1): 0.0);
}

Quantity MVDirection::positionAngle(const MVPosition &other, 
				    const Unit &unit) const {
  return Quantity(positionAngle(other), "rad").get(unit);
}

Quantity MVDirection::positionAngle(const MVDirection &other, 
				    const Unit &unit) const {
  return Quantity(positionAngle(other), "rad").get(unit);
}

Double MVDirection::separation(const MVPosition &other) const {
  const Vector<Double> &otherxyz = other.getValue();
  Double l2(norm(otherxyz));
  l2 = l2 > 0 ? l2 : 1.0;
  Double d1 = std::sqrt(square(xyz(0) - otherxyz(0)/l2) + 
		   square(xyz(1) - otherxyz(1)/l2) +
		   square(xyz(2) - otherxyz(2)/l2))/2.0; 
  return 2*std::asin(d1 < 1.0 ? d1 : 1.0);
}

Double MVDirection::separation(const MVDirection &other) const {
  Double d1 = std::sqrt(square(xyz(0) - other.xyz(0)) + 
		   square(xyz(1) - other.xyz(1)) +
		   square(xyz(2) - other.xyz(2)))/2.0; 
  return 2*std::asin(d1 < 1.0 ? d1 : 1.0);
}

Quantity MVDirection::separation(const MVPosition &other, 
				 const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

Quantity MVDirection::separation(const MVDirection &other, 
				 const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

MVDirection MVDirection::crossProduct(const MVDirection &other) const {
  MVDirection res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

void MVDirection::shift(const Quantum<Double> &lng,
			const Quantum<Double> &lat, Bool trueAngle) {
  shift(lng.getBaseValue(), lat.getBaseValue(), trueAngle);
}

void MVDirection::shift(Double lng, Double lat, Bool trueAngle) {
  Vector<Double> x(2);
  x = get();
  if (trueAngle) {
  // The following calculation could maybe done quicker, but for now this
  // is more transparent.
    RotMatrix rm=RotMatrix(Euler(-lng, 3, x(1)+lat, 2, -x(0), 3));
    *this = MVDirection(1,0,0) * rm;
  } else {
    x(0) += lng;
    x(1) += lat;
    *this = MVDirection(x);
  }
}

void MVDirection::shiftLongitude(const Quantum<Double> &lng, Bool trueAngle) {
  shift(lng.getBaseValue(), 0, trueAngle);
}

void MVDirection::shiftLongitude(Double lng, Bool trueAngle) {
  shift(lng, 0, trueAngle);
}

void MVDirection::shiftLatitude(const Quantum<Double> &lat, 
				Bool trueAngle) {
  shift(0, lat.getBaseValue(), trueAngle);
}

void MVDirection::shiftLatitude(Double lat, Bool trueAngle) {
  shift(0, lat, trueAngle);
}

void MVDirection::shift(const MVDirection &shft, Bool trueAngle) {
  Vector<Double> x(2);
  x = shft.get();
  shift(x(0), x(1), trueAngle);
}

void MVDirection::shiftAngle(const Quantum<Double> &off,
			     const Quantum<Double> &pa) {
  shiftAngle(off.getBaseValue(), pa.getBaseValue());
}

void MVDirection::shiftAngle(Double off, Double pa) {
  Vector<Double> x(2);
  x = get();
  Double nlat = std::asin(std::cos(off)*std::sin(x(1)) + std::sin(off)*std::cos(x(1))*std::cos(pa));
  Double nlng = std::sin(off)*std::sin(pa);
  if (std::cos(nlat) != 0) {
    nlng = std::asin(nlng/std::cos(nlat));
  } else nlng = 0;
  *this = MVDirection(x(0)+nlng, nlat);
}

MeasValue *MVDirection::clone() const {
  return (new MVDirection(*this));
}


MVDirection operator*(const RotMatrix &left, const MVDirection &right) {
  MVDirection result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVDirection operator*(const MVDirection &left, const RotMatrix &right) {
  MVDirection result(left);
  result *= right;
  return result;
}

} //# NAMESPACE CASACORE - END

