//# MVPosition.cc: A 3D vector in space
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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double MVPosition::loLimit = 743.568;
const Double MVPosition::hiLimit = 743.569;

//# Constructors
MVPosition::MVPosition() :
  xyz(3) {
    xyz = Double(0.0);
}

MVPosition::MVPosition(const MVPosition &other) : 
  MeasValue(),
  xyz(3)
{
  xyz = other.xyz;
}

MVPosition &MVPosition::operator=(const MVPosition &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVPosition::MVPosition(Double in) :
  xyz(3) {
    xyz = Double(0.0);
    xyz(2) = in;
  }

MVPosition::MVPosition(const Quantity &l) :
  xyz(3) {
    xyz = Double(0.0);
    l.assure(UnitVal::LENGTH);
    xyz(2) = l.getBaseValue();
  }

MVPosition::MVPosition(Double in0, Double in1, Double in2) : 
  xyz(3) {
    xyz(0) = in0;
    xyz(1) = in1;
    xyz(2) = in2;
  }

MVPosition::MVPosition(const Quantity &l, Double angle0, Double angle1) : 
  xyz(3) {
  Double loc = std::cos(angle1);
  xyz(0) = std::cos(angle0)*loc;
  xyz(1) = std::sin(angle0)*loc;
  xyz(2) = std::sin(angle1);
  Double t(l.getBaseValue());
  if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
  else if (t>loLimit && t<hiLimit) t += 0.001;
  readjust(t);
}

MVPosition::MVPosition(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  xyz(3) {
  Double loc = (cos(angle1)).getValue();
  xyz(0) = ((cos(angle0)).getValue()) * loc;
  xyz(1) = ((sin(angle0)).getValue()) * loc;
  xyz(2) = (sin(angle1)).getValue();
  l.assure(UnitVal::LENGTH);
  Double t(l.getBaseValue());
  if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
  else if (t>loLimit && t<hiLimit) t += 0.001;
  readjust(t);
}

MVPosition::MVPosition(const Quantum<Vector<Double> > &angle) :
  xyz(3) {
  uInt i; i = angle.getValue().nelements();
  if (i > 3 ) {
    throw (AipsError("Illegeal vector length in MVPosition constructor"));
  } else if (i == 3) {
    angle.assure(UnitVal::LENGTH);
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

MVPosition::MVPosition(const Quantity &l, 
		       const Quantum<Vector<Double> > &angle) :
  xyz(3) {
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVPosition constructor"));
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
    l.assure(UnitVal::LENGTH);
    readjust(l.getBaseValue());
  }

MVPosition::MVPosition(const Vector<Double> &other) :
  xyz(3) {
    uInt i; i = other.nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVPosition constructor"));
    } else if (i == 3) {
      xyz = other;
    } else {
      Vector<Double> tsin = (sin(other));
      Vector<Double> tcos = (cos(other));
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

MVPosition::MVPosition(const Vector<Quantity> &other) :
  xyz(3) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantum vector in MVPosition constructor"));
  }
}

//# Destructor
MVPosition::~MVPosition() {}

//# Operators
Bool MVPosition::
operator==(const MVPosition &other) const {
  return (allEQ(xyz, other.xyz));
}

Bool MVPosition::
operator!=(const MVPosition &other) const {
  return (!(*this == other));
}

Bool MVPosition::
near(const MVPosition &other, Double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

Bool MVPosition::
near(const MVPosition &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVPosition::
nearAbs(const MVPosition &other, Double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

Double MVPosition::
operator*(const MVPosition &other) const {
  Double tmp = 0.0;
  for (Int i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

Double &MVPosition::operator()(uInt which) {
  DebugAssert(which < 3, AipsError);
  return (xyz(which));
}

const Double &MVPosition::operator()(uInt which) const {
  DebugAssert(which < 3, AipsError);
  return (xyz(which));
}

MVPosition MVPosition::operator-() const {
  MVPosition tmp; tmp = *this;
  tmp.xyz = -xyz;
  return tmp;
}

MVPosition &MVPosition::operator+=(const MVPosition &right) {
  xyz += right.xyz;
  return *this;
}

MVPosition MVPosition::operator+(const MVPosition &right) const {
  MVPosition tmp = *this;
  tmp += right;
  return tmp;
}

MVPosition &MVPosition::operator-=(const MVPosition &right) {
  xyz -= right.xyz;
  return *this;
}

MVPosition MVPosition::operator-(const MVPosition &right) const{
  MVPosition tmp = *this;
  tmp -= right;
  return tmp;
}

MVPosition &MVPosition::operator*=(const RotMatrix &right) {
  MVPosition result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += xyz(j) * right(j,i);
    }
  }
  *this = result;
  return *this;
}

MVPosition &MVPosition::operator*=(Double right) {
  for (Int j=0; j<3; j++) {
    xyz(j) *= right;
  }
  return *this;
}

//# Member functions

uInt MVPosition::type() const {
  return Register(static_cast<MVPosition *>(0));
}

void MVPosition::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVPosition *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVPosition"));
  }
}

void MVPosition::adjust() {}

void MVPosition::adjust(Double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVPosition::readjust(Double res) {
  if (res == 0.0) {
    xyz *= 1e-6;
  } else {
    xyz *= res;
  }
}

Double MVPosition::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<Double> MVPosition::get() const{
  Vector<Double> tmp(3);
  tmp(0) = norm(xyz);
  tmp(1) = getLong();
  tmp(2) = getLat(tmp(0));
  if (tmp(0)>loLimit && tmp(0)<hiLimit) {
    tmp(0) = (tmp(0) - hiLimit)*1e7;
  }
  return tmp;
}

const Vector<Double> &MVPosition::getValue() const {
  return xyz;
}

Quantum<Vector<Double> > MVPosition::getAngle() const{
  Vector<Double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<Double> >(tmp,"rad");
}

Quantum<Vector<Double> > MVPosition::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

Double MVPosition::getLong() const {
  return ((xyz(0) != 0 || xyz(1) != 0) ?
	  std::atan2(xyz(1),xyz(0)) : 0.0);
}

Quantity MVPosition::getLong(const Unit &unit) const {
  return (Quantity(getLong(), "rad").get(unit));
}

Double MVPosition::getLat() const {
  return getLat(norm(xyz));
}

Double MVPosition::getLat(Double ln) const {
  return std::asin(xyz(2)/((ln == 0) ? 1.0 : ln));
}

Quantity MVPosition::getLat(const Unit &unit) const {
  return (Quantity(getLat(), "rad").get(unit));
}

Quantity MVPosition::getLength() const{
  Double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp, "m");
}

Quantity MVPosition::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVPosition::positionAngle(const MVPosition &other) const {
  Double longDiff(getLong() - other.getLong());
  Double ln(norm(xyz)); 
  Double slat1(xyz(2)/ln);
  ln = norm(other.xyz);
  Double slat2(other.xyz(2)/ln);
  Double clat2(std::sqrt(std::fabs(1.0 - slat2*slat2)));
  Double s1(-clat2 * std::sin(longDiff));
  Double c1(std::sqrt(std::fabs(1.0 - slat1*slat1))*slat2 - slat1*clat2*std::cos(longDiff));
  return ((s1 != 0 || c1 != 0) ? std::atan2(s1, c1): 0.0);
}

Quantity MVPosition::positionAngle(const MVPosition &other, 
				   const Unit &unit) const {
  return Quantity(positionAngle(other), "rad").get(unit);
}

Double MVPosition::separation(const MVPosition &other) const {
  Double l1(norm(xyz));
  l1 = l1 > 0 ? l1 : 1.0; 
  Double l2(norm(other.xyz));
  l2 = l2 > 0 ? l2 : 1.0;
  Double d1 = std::sqrt(square(xyz(0)/l1 - other.xyz(0)/l2) + 
		   square(xyz(1)/l1 - other.xyz(1)/l2) +
		   square(xyz(2)/l1 - other.xyz(2)/l2))/2.0; 
  return 2*std::asin(d1 < 1.0 ? d1 : 1.0);
}

Quantity MVPosition::separation(const MVPosition &other, 
				const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

MVPosition MVPosition::crossProduct(const MVPosition &other) const {
  MVPosition res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

void MVPosition::print(ostream &os) const {
  os << getValue();
}

MeasValue *MVPosition::clone() const {
  return (new MVPosition(*this));
}

Vector<Double> MVPosition::getVector() const {
  return xyz;
}

void MVPosition::putVector(const Vector<Double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uInt i=0; i<in.nelements();i++) xyz(i) = in(i);
  }
}

Vector<Quantum<Double> > MVPosition::getRecordValue() const {
  Vector<Double> t(3);
  t = get();
  Vector<Quantum<Double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<Double> > MVPosition::getXRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

Bool MVPosition::putValue(const Vector<Quantum<Double> > &in) {
  uInt i; i = in.nelements();
  if (i != 3 ) return False;
  if (in(0).check(UnitVal::LENGTH)) {
    if (in(1).check(UnitVal::LENGTH) &&
	in(2).check(UnitVal::LENGTH)) {
      uInt j;
      for (j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      }
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      uInt j;
      for (j=1; j < i; j++) {
	tsin(j-1) = (sin(in(j))).getValue(); 
	tcos(j-1) = (cos(in(j))).getValue(); 
      }
      xyz = Double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      Double t(in(0).getBaseValue());
      if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
      else if (t>loLimit && t<hiLimit) t += 0.001;
      readjust(t);
    } else {
      return False;
    }
  } else if (in(2).check(UnitVal::LENGTH)) {
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
      Double t(in(2).getBaseValue());
      if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
      else if (t>loLimit && t<hiLimit) t += 0.001;
      readjust(t);
    } else {
      return False;
    }
  } else {
    return False;
  }
  return True;
}

MVPosition operator*(const RotMatrix &left, const MVPosition &right) {
  MVPosition result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVPosition operator*(const MVPosition &left, const RotMatrix &right) {
  MVPosition result(left);
  result *= right;
  return result;
}

MVPosition operator*(Double left, const MVPosition &right) {
  MVPosition result(right);
  result *= left;
  return result;
}

MVPosition operator*(const MVPosition &left, Double right) {
  MVPosition result(left);
  result *= right;
  return result;
}

Double operator*(const Vector<Double> &left, const MVPosition &right) {
  MVPosition tmp(left);
  return (tmp * right);
}

Double operator*(const MVPosition &left, const Vector<Double> &right) {
  MVPosition tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

