//# MVPosition.cc: A 3D vector in space
//# Copyright (C) 1996,1997,1998
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
typedef Quantum<Double> gpp_mvposition_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <aips/RTTI/Register.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/QMath.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

// MVPosition class

//# Constructors
MVPosition::MVPosition() :
  xyz(3) {
    xyz = Double(0.0);
}

MVPosition::MVPosition(const MVPosition &other) : 
  xyz(3) {
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
    l.assert(UnitVal::LENGTH);
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
    Double loc = cos(angle1);
    xyz(0) = cos(angle0)*loc;
    xyz(1) = sin(angle0)*loc;
    xyz(2) = sin(angle1);
    readjust(l.getBaseValue());
  }

MVPosition::MVPosition(const Quantity &l, const Quantity &angle0, 
		       const Quantity &angle1) : 
  xyz(3) {
    Double loc = (cos(angle1)).getValue();
    xyz(0) = ((cos(angle0)).getValue()) * loc;
    xyz(1) = ((sin(angle0)).getValue()) * loc;
    xyz(2) = (sin(angle1)).getValue();
    l.assert(UnitVal::LENGTH);
    readjust(l.getBaseValue());
  }

MVPosition::MVPosition(const Quantum<Vector<Double> > &angle) :
  xyz(3) {
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegeal vector length in MVPosition constructor"));
    } else if (i == 3) {
      angle.assert(UnitVal::LENGTH);
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
      angle.assert(UnitVal::NODIM);
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
    l.assert(UnitVal::LENGTH);
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
      Vector<Double> tsin = (sin(other.ac()));
      Vector<Double> tcos = (cos(other.ac()));
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
    };
  }

//# Destructor
MVPosition::~MVPosition() {}

//# Operators
Bool MVPosition::
operator==(const MVPosition &other) const {
  return (allEQ(xyz.ac(), other.xyz.ac()));
}

Bool MVPosition::
operator!=(const MVPosition &other) const {
  return ToBool(!(*this == other));
}

Bool MVPosition::
near(const MVPosition &other, Double tol) const {
  return (allNear(xyz.ac(), other.xyz.ac(), tol));
}

Bool MVPosition::
near(const MVPosition &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVPosition::
nearAbs(const MVPosition &other, Double tol) const {
  return (allNearAbs(xyz.ac(), other.xyz.ac(), tol));
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
  tmp.xyz.ac() = -xyz.ac();
  return tmp;
}

MVPosition &MVPosition::operator+=(const MVPosition &right) {
  xyz.ac() += right.xyz.ac();
  return *this;
}

MVPosition MVPosition::operator+(const MVPosition &right) const {
  MVPosition tmp = *this;
  tmp += right;
  return tmp;
}

MVPosition &MVPosition::operator-=(const MVPosition &right) {
  xyz.ac() -= right.xyz.ac();
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
  return Register((MVPosition *)0);
}

void MVPosition::assert(const MeasValue &in) {
  if (in.type() != Register((MVPosition *)0)) {
    throw(AipsError("Illegal MeasValue type argument: MVPosition"));
  };
}

void MVPosition::adjust() {}

void MVPosition::adjust(Double &res) {
  res = sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz.ac() /= res;
  };
}

void MVPosition::readjust(Double res) {
  if (res == 0.0) {
    xyz.ac() *= 1e-6;
  } else {
    xyz.ac() *= res;
  };
}

Double MVPosition::radius() {
  return (sqrt(operator*(*this)));
}

Vector<Double> MVPosition::get() const{
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

Quantity MVPosition::getLength() const{
  Double tmp = sqrt(operator*(*this));
  return Quantity(tmp,"m");
}

Quantity MVPosition::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVPosition::positionAngle(const MVPosition &other) const {
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

Quantity MVPosition::positionAngle(const MVPosition &other, 
				   const Unit &unit) const {
  return Quantity(positionAngle(other), "rad").get(unit);
}

Double MVPosition::separation(const MVPosition &other) const {
  MVPosition t1(*this);
  MVPosition t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  Double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*asin(d1));
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
  os << getValue().ac();
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
  };
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
      };
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      uInt j;
      for (j=1; j < i; j++) {
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
  } else {
    return False;
  };
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
