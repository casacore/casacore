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

//# Includes
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const double MVPosition::loLimit = 743.568;
const double MVPosition::hiLimit = 743.569;

//# Constructors
MVPosition::MVPosition() :
  xyz(3)
{
  xyz = double(0.0);
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

MVPosition::MVPosition(double in) :
  xyz(3)
{
  xyz = double(0.0);
  xyz(2) = in;
}

MVPosition::MVPosition(const Quantity &l) :
  xyz(3)
{
  xyz = double(0.0);
  l.assure(UnitVal::LENGTH);
  xyz(2) = l.getBaseValue();
}

MVPosition::MVPosition(double in0, double in1, double in2) : 
  xyz(3) 
{
  xyz(0) = in0;
  xyz(1) = in1;
  xyz(2) = in2;
}

MVPosition::MVPosition(const Quantity &l, double angle0, double angle1) : 
  xyz(3) {
  double loc = std::cos(angle1);
  xyz(0) = std::cos(angle0)*loc;
  xyz(1) = std::sin(angle0)*loc;
  xyz(2) = std::sin(angle1);
  double t(l.getBaseValue());
  if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
  else if (t>loLimit && t<hiLimit) t += 0.001;
  readjust(t);
}

MVPosition::MVPosition(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  xyz(3) {
  double loc = (cos(angle1)).getValue();
  xyz(0) = ((cos(angle0)).getValue()) * loc;
  xyz(1) = ((sin(angle0)).getValue()) * loc;
  xyz(2) = (sin(angle1)).getValue();
  l.assure(UnitVal::LENGTH);
  double t(l.getBaseValue());
  if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
  else if (t>loLimit && t<hiLimit) t += 0.001;
  readjust(t);
}

MVPosition::MVPosition(const Quantum<Vector<double> > &angle) :
  xyz(3) {
  uint32_t i; i = angle.getValue().nelements();
  if (i > 3 ) {
    throw (AipsError("Illegeal vector length in MVPosition constructor"));
  } else if (i == 3) {
    angle.assure(UnitVal::LENGTH);
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

MVPosition::MVPosition(const Quantity &l, 
		       const Quantum<Vector<double> > &angle) :
  xyz(3) {
    uint32_t i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVPosition constructor"));
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
    l.assure(UnitVal::LENGTH);
    readjust(l.getBaseValue());
  }

MVPosition::MVPosition(const Vector<double> &other) :
  xyz(3) {
    uint32_t i; i = other.nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVPosition constructor"));
    } else if (i == 3) {
      xyz = other;
    } else {
      Vector<double> tsin = (sin(other));
      Vector<double> tcos = (cos(other));
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

MVPosition::MVPosition(const Vector<Quantity> &other) :
  xyz(3) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantum vector in MVPosition constructor"));
  }
}

//# Destructor
MVPosition::~MVPosition()
{
}

//# Operators
bool MVPosition::
operator==(const MVPosition &other) const {
  return (allEQ(xyz, other.xyz));
}

bool MVPosition::
operator!=(const MVPosition &other) const {
  return (!(*this == other));
}

bool MVPosition::
near(const MVPosition &other, double tol) const {
  return (allNear(xyz, other.xyz, tol));
}

bool MVPosition::
near(const MVPosition &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

bool MVPosition::
nearAbs(const MVPosition &other, double tol) const {
  return (allNearAbs(xyz, other.xyz, tol));
}

double MVPosition::
operator*(const MVPosition &other) const {
  double tmp = 0.0;
  for (int32_t i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

double &MVPosition::operator()(uint32_t which) {
  DebugAssert(which < 3, AipsError);
  return (xyz(which));
}

const double &MVPosition::operator()(uint32_t which) const {
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
  double x = xyz(0);
  double y = xyz(1);
  double z = xyz(2);
  xyz(0) = x * right(0, 0) + y * right(1, 0) + z * right(2, 0);
  xyz(1) = x * right(0, 1) + y * right(1, 1) + z * right(2, 1);
  xyz(2) = x * right(0, 2) + y * right(1, 2) + z * right(2, 2);
  return *this;
}

MVPosition &MVPosition::operator*=(double right) {
  for (int32_t j=0; j<3; j++) {
    xyz(j) *= right;
  }
  return *this;
}

//# Member functions

void MVPosition::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVPosition*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVPosition"));
  }
}

void MVPosition::adjust() {}

void MVPosition::adjust(double &res) {
  res = std::sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz /= res;
  }
}

void MVPosition::readjust(double res) {
  if (res == 0.0) {
    xyz *= 1e-6;
  } else {
    xyz *= res;
  }
}

double MVPosition::radius() {
  return (std::sqrt(operator*(*this)));
}

Vector<double> MVPosition::get() const{
  Vector<double> tmp(3);
  tmp(0) = norm(xyz);
  tmp(1) = getLong();
  tmp(2) = getLat(tmp(0));
  if (tmp(0)>loLimit && tmp(0)<hiLimit) {
    tmp(0) = (tmp(0) - hiLimit)*1e7;
  }
  return tmp;
}

const Vector<double> &MVPosition::getValue() const {
  return xyz;
}

Quantum<Vector<double> > MVPosition::getAngle() const{
  Vector<double> tp(3), tmp(2);
  tp = get();
  tmp(0) = tp(1);
  tmp(1) = tp(2);
  return Quantum<Vector<double> >(tmp,"rad");
}

Quantum<Vector<double> > MVPosition::getAngle(const Unit &unit) const{
  return getAngle().get(unit);
}

double MVPosition::getLong() const {
  return ((xyz(0) != 0 || xyz(1) != 0) ?
	  std::atan2(xyz(1),xyz(0)) : 0.0);
}

Quantity MVPosition::getLong(const Unit &unit) const {
  return (Quantity(getLong(), "rad").get(unit));
}

double MVPosition::getLat() const {
  return getLat(norm(xyz));
}

double MVPosition::getLat(double ln) const {
  return std::asin(xyz(2)/((ln == 0) ? 1.0 : ln));
}

Quantity MVPosition::getLat(const Unit &unit) const {
  return (Quantity(getLat(), "rad").get(unit));
}

Quantity MVPosition::getLength() const{
  double tmp = std::sqrt(operator*(*this));
  return Quantity(tmp, "m");
}

Quantity MVPosition::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

double MVPosition::positionAngle(const MVPosition &other) const {
  double longDiff(getLong() - other.getLong());
  double ln(norm(xyz)); 
  double slat1(xyz(2)/ln);
  ln = norm(other.xyz);
  double slat2(other.xyz(2)/ln);
  double clat2(std::sqrt(std::fabs(1.0 - slat2*slat2)));
  double s1(-clat2 * std::sin(longDiff));
  double c1(std::sqrt(std::fabs(1.0 - slat1*slat1))*slat2 - slat1*clat2*std::cos(longDiff));
  return ((s1 != 0 || c1 != 0) ? std::atan2(s1, c1): 0.0);
}

Quantity MVPosition::positionAngle(const MVPosition &other, 
				   const Unit &unit) const {
  return Quantity(positionAngle(other), "rad").get(unit);
}

double MVPosition::separation(const MVPosition &other) const {
  double l1(norm(xyz));
  l1 = l1 > 0 ? l1 : 1.0; 
  double l2(norm(other.xyz));
  l2 = l2 > 0 ? l2 : 1.0;
  double d1 = std::sqrt(square(xyz(0)/l1 - other.xyz(0)/l2) + 
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

Vector<double> MVPosition::getVector() const {
  return xyz;
}

void MVPosition::putVector(const Vector<double> &in) {
  if (in.nelements() == 3) {
    xyz = in;
  } else {
    xyz = 0.0;
    for (uint32_t i=0; i<in.nelements();i++) xyz(i) = in(i);
  }
}

Vector<Quantum<double> > MVPosition::getRecordValue() const {
  Vector<double> t(3);
  t = get();
  Vector<Quantum<double> > tmp(3);
  tmp(2) = Quantity(t(0), "m");
  tmp(0) = Quantity(t(1), "rad"); 
  tmp(1) = Quantity(t(2), "rad"); 
  return tmp;
}

Vector<Quantum<double> > MVPosition::getXRecordValue() const {
  Vector<Quantum<double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "m");
  tmp(1) = Quantity(xyz(1), "m");
  tmp(2) = Quantity(xyz(2), "m");
  return tmp;
}

bool MVPosition::putValue(const Vector<Quantum<double> > &in) {
  uint32_t i; i = in.nelements();
  if (i != 3 ) return false;
  if (in(0).check(UnitVal::LENGTH)) {
    if (in(1).check(UnitVal::LENGTH) &&
	in(2).check(UnitVal::LENGTH)) {
      uint32_t j;
      for (j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      }
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<double> tsin(2), tcos(2);
      uint32_t j;
      for (j=1; j < i; j++) {
	tsin(j-1) = (sin(in(j))).getValue(); 
	tcos(j-1) = (cos(in(j))).getValue(); 
      }
      xyz = double(0.0);
      xyz(0) = tcos(0) * tcos(1);
      xyz(1) = tsin(0) * tcos(1);
      xyz(2) = tsin(1);
      double t(in(0).getBaseValue());
      if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
      else if (t>loLimit && t<hiLimit) t += 0.001;
      readjust(t);
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
      double t(in(2).getBaseValue());
      if (t<0 && t>-7.0e6) t = t/1.0e7 + hiLimit;
      else if (t>loLimit && t<hiLimit) t += 0.001;
      readjust(t);
    } else {
      return false;
    }
  } else {
    return false;
  }
  return true;
}

MVPosition operator*(const RotMatrix &left, const MVPosition &right) {
  MVPosition result;
  for (int32_t i=0; i<3; i++) {
    result(i) = 0;
    for (int32_t j=0; j<3; j++) {
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

MVPosition operator*(double left, const MVPosition &right) {
  MVPosition result(right);
  result *= left;
  return result;
}

MVPosition operator*(const MVPosition &left, double right) {
  MVPosition result(left);
  result *= right;
  return result;
}

double operator*(const Vector<double> &left, const MVPosition &right) {
  MVPosition tmp(left);
  return (tmp * right);
}

double operator*(const MVPosition &left, const Vector<double> &right) {
  MVPosition tmp(right);
  return (tmp * left);
}

} //# NAMESPACE CASACORE - END

