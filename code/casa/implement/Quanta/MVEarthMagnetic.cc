//# MVEarthMagnetic.cc: A 3D Earth magnetic field vector
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
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_mvEarthMagnetic_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <trial/Measures/MVEarthMagnetic.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/UnitVal.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/QLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

// MVEarthMagnetic class

//# Constructors
MVEarthMagnetic::MVEarthMagnetic() :
  xyz(3) {
    xyz = Double(0.0);
}

MVEarthMagnetic::MVEarthMagnetic(const MVEarthMagnetic &other) : 
  xyz(3) {
    xyz = other.xyz;
  }
MVEarthMagnetic &MVEarthMagnetic::operator=(const MVEarthMagnetic &other) {
  if (this != &other) {
    xyz = other.xyz;
  }
  return *this;
}

MVEarthMagnetic::MVEarthMagnetic(Double in) :
  xyz(3) {
    xyz = Double(0.0);
    xyz(2) = in;
  }

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l) :
  xyz(3) {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    xyz = Double(0.0);
    l.assert(testUnit);
    xyz(2) = l.getBaseValue();
  }

MVEarthMagnetic::MVEarthMagnetic(Double in0, Double in1, Double in2) : 
xyz(3) {
    xyz(0) = in0;
    xyz(1) = in1;
    xyz(2) = in2;
}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, Double angle0, Double angle1) : 
xyz(3) {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    l.assert(testUnit);
    Double loc = cos(angle1);
    xyz(0) = cos(angle0)*loc;
    xyz(1) = sin(angle0)*loc;
    xyz(2) = sin(angle1);
    readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Quantity &l, const Quantity &angle0, 
		     const Quantity &angle1) : 
xyz(3) {
  static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
    UnitVal::CURRENT;
  l.assert(testUnit);
  Double loc = (cos(angle1)).getValue();
  xyz(0) = ((cos(angle0)).getValue()) * loc;
  xyz(1) = ((sin(angle0)).getValue()) * loc;
  xyz(2) = (sin(angle1)).getValue();
  readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Quantum<Vector<Double> > &angle) :
  xyz(3) {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
    } else if (i == 3) {
      angle.assert(testUnit);
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
xyz(3) {
    static UnitVal testUnit = UnitVal::MASS/UnitVal::TIME/UnitVal::TIME/
      UnitVal::CURRENT;
    uInt i; i = angle.getValue().nelements();
    if (i > 3 ) {
      throw (AipsError("Illegal vector length in MVEarthMagnetic constructor"));
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
    l.assert(testUnit);
    readjust(l.getBaseValue());
}

MVEarthMagnetic::MVEarthMagnetic(const Vector<Double> &other) :
xyz(3) {
    uInt i; i = other.nelements();
    if (i > 3 ) {
	throw (AipsError("Illegal vector length in MVEarthMagnetic condtructor"));
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

MVEarthMagnetic::MVEarthMagnetic(const Vector<Quantity> &other) :
  xyz(3) {
    if (!putValue(other)) {
      throw (AipsError("Illegal quantity vector in MVEarthMagnetic constructor"));
    };
  }

//# Destructor
MVEarthMagnetic::~MVEarthMagnetic() {}

//# Operators
Bool MVEarthMagnetic::
operator==(const MVEarthMagnetic &other) const {
  return (allEQ(xyz.ac(), other.xyz.ac()));
}

Bool MVEarthMagnetic::
operator!=(const MVEarthMagnetic &other) const {
  return ToBool(!(*this == other));
}

Bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, Double tol) const {
  return (allNear(xyz.ac(), other.xyz.ac(), tol));
}

Bool MVEarthMagnetic::
near(const MVEarthMagnetic &other, Quantity tol) const {
  return (separation(other,"rad") <= tol);
}

Bool MVEarthMagnetic::
nearAbs(const MVEarthMagnetic &other, Double tol) const {
  return (allNearAbs(xyz.ac(), other.xyz.ac(), tol));
}

Double MVEarthMagnetic::
operator*(const MVEarthMagnetic &other) const {
  Double tmp = 0.0;
  for (Int i=0; i<3; i++) {
    tmp += xyz(i) * other.xyz(i);
  }
  return tmp;
}

Double &MVEarthMagnetic::operator()(uInt which) {
  DebugAssert(which < 3, AipsError);
  return (xyz(which));
}

const Double &MVEarthMagnetic::operator()(uInt which) const {
  DebugAssert(which < 3, AipsError);
  return (xyz(which));
}

MVEarthMagnetic MVEarthMagnetic::operator-() const {
  MVEarthMagnetic tmp; tmp = *this;
  tmp.xyz.ac() = -xyz.ac();
  return tmp;
}

MVEarthMagnetic &MVEarthMagnetic::operator+=(const MVEarthMagnetic &right) {
  xyz.ac() += right.xyz.ac();
  return *this;
}

MVEarthMagnetic MVEarthMagnetic::operator+(const MVEarthMagnetic &right) const {
  MVEarthMagnetic tmp = *this;
  tmp += right;
  return tmp;
}

MVEarthMagnetic &MVEarthMagnetic::operator-=(const MVEarthMagnetic &right) {
  xyz.ac() -= right.xyz.ac();
  return *this;
}

MVEarthMagnetic MVEarthMagnetic::operator-(const MVEarthMagnetic &right) const{
  MVEarthMagnetic tmp = *this;
  tmp -= right;
  return tmp;
}

MVEarthMagnetic &MVEarthMagnetic::operator*=(const RotMatrix &right) {
    MVEarthMagnetic result;
    for (Int i=0; i<3; i++) {
      result(i) = 0;
      for (Int j=0; j<3; j++) {
	result(i) += xyz(j) * right(j,i);
      }
    }
    *this = result;
    return *this;
}

MVEarthMagnetic &MVEarthMagnetic::operator*=(Double right) {
  for (Int j=0; j<3; j++) {
    xyz(j) *= right;
  }
  return *this;
}

//# Member functions

uInt MVEarthMagnetic::type() const {
  return Register((MVEarthMagnetic *)0);
}

void MVEarthMagnetic::assert(const MeasValue &in) {
  if (in.type() != Register((MVEarthMagnetic *)0)) {
    throw(AipsError("Illegal MeasValue type argument: MVEarthMagnetic"));
  };
}
void MVEarthMagnetic::adjust() {}

void MVEarthMagnetic::adjust(Double &res) {
  res = sqrt(operator*(*this));
  if (res != 0.0 && res != 1.0) {
    xyz.ac() /= res;
  };
}

void MVEarthMagnetic::readjust(Double res) {
  if (res == 0.0) {
    xyz.ac() *= 1e-6;
  } else {
    xyz.ac() *= res;
  };
}

Double MVEarthMagnetic::radius() {
  return (sqrt(operator*(*this)));
}

Vector<Double> MVEarthMagnetic::get() const{
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
  Double tmp = sqrt(operator*(*this));
  return Quantity(tmp,"T");
}

Quantity MVEarthMagnetic::getLength(const Unit &unit) const {
  return getLength().get(unit);
}

Double MVEarthMagnetic::EarthMagneticAngle(const MVEarthMagnetic &other) const {
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

Quantity MVEarthMagnetic::EarthMagneticAngle(const MVEarthMagnetic &other, 
					     const Unit &unit) const {
  return Quantity(EarthMagneticAngle(other), "rad").get(unit);
}

Double MVEarthMagnetic::separation(const MVEarthMagnetic &other) const {
  MVEarthMagnetic t1(*this);
  MVEarthMagnetic t2(other);
  t1.adjust(); t2.adjust();
  t1 -= t2;
  Double d1 = t1.radius()/2.0;
  d1 = (d1 < 1.0 ? d1 : 1.0);
  return (2*asin(d1));
}

Quantity MVEarthMagnetic::separation(const MVEarthMagnetic &other, 
				     const Unit &unit) const {
  return Quantity(separation(other), "rad").get(unit);
}

void MVEarthMagnetic::print(ostream &os) const {
  os << getValue().ac();
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
    for (Int i=0; i<in.nelements(); i++) xyz(i) = in(i);
  };
}

Vector<Quantum<Double> > MVEarthMagnetic::getRecordValue() const {
  Vector<Quantum<Double> > tmp(3);
  tmp(0) = Quantity(xyz(0), "T");
  tmp(1) = Quantity(xyz(1), "T");
  tmp(2) = Quantity(xyz(2), "T");
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
      for (Int j = 0; j<i; j++) {
	xyz(j) = in(j).getBaseValue();
      };
    } else if (in(1).check(UnitVal::ANGLE) &&
	       in(2).check(UnitVal::ANGLE)) {
      Vector<Double> tsin(2), tcos(2);
      for (Int j=1; j < i; j++) {
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
  } else {
    return False;
  };
  return True;
}

MVEarthMagnetic operator*(const RotMatrix &left, const MVEarthMagnetic &right) {
  MVEarthMagnetic result;
  for (Int i=0; i<3; i++) {
    result(i) = 0;
    for (Int j=0; j<3; j++) {
      result(i) += left(i,j) * right(j);
    }
  }
  return result;
}

MVEarthMagnetic operator*(const MVEarthMagnetic &left, const RotMatrix &right) {
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
