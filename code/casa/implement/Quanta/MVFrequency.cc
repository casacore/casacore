//# MVFrequency.cc: Internal value for MFrequency
//# Copyright (C) 1996,1997
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
typedef Quantum<Double> gpp_mvfrequency_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MVFrequency.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/QC.h>

// MVFrequency class

//# Constructors
MVFrequency::MVFrequency() : 
  val(0.0){}

MVFrequency::MVFrequency(Double d) : 
  val(d){}

MVFrequency::MVFrequency(const MVFrequency &other) :
  val(other.val) {}

MVFrequency::MVFrequency(const Quantity &other) {
  val = makeF(other.getValue(), other.getFullUnit());
}

MVFrequency::MVFrequency(const Quantum<Vector<Double> > &other) {
  Vector<Double> tmp;
  tmp = other.getValue();
  uInt i = tmp.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = makeF(tmp(0), other.getFullUnit());
  } else {
    throw (AipsError("Illegal vector length in MVFrequency constructor"));
  };
}

MVFrequency::MVFrequency(const Vector<Double> &other) {
  uInt i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = other(0);
  } else {
    throw (AipsError("Illegal vector length in MVFrequency constructor"));
  };
}

MVFrequency::MVFrequency(const Vector<Quantity> &other) {
  uInt i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = makeF( other(0).getValue(), other(0).getFullUnit());
  } else {
    throw (AipsError("Illegal vector length in MVFrequency constructor"));
  };
}

MVFrequency &MVFrequency::operator=(const MVFrequency &other) {
  if (this != &other) {
    val = other.val;
  }
  return *this;
}

// Destructor
MVFrequency::~MVFrequency() {}

// Operators
MVFrequency::operator Double() const {
  return val;
}

MVFrequency &MVFrequency::operator+=(const MVFrequency &other) {
  val += other.val;
  return *this;
}

MVFrequency &MVFrequency::operator-=(const MVFrequency &other) {
  val -= other.val;
  return *this;
}

Bool MVFrequency::operator==(const MVFrequency &other) const {
  return ToBool(val == other.val);
}

Bool MVFrequency::operator!=(const MVFrequency &other) const {
  return ToBool(val != other.val);
}

Bool MVFrequency::near(const MVFrequency &other, Double tol) const {
  return ::near(val, other.val, tol);
}

Bool MVFrequency::nearAbs(const MVFrequency &other, Double tol) const {
  return ::nearAbs(val, other.val, tol);
}

// Member functions

uInt MVFrequency::type() const {
  return Register((MVFrequency *)0);
}

void MVFrequency::assert(const MeasValue &in) {
  if (in.type() != Register((MVFrequency *)0)) {
    throw(AipsError("Illegal MeasValue type argument: MVFrequency"));
  };
}

void MVFrequency::print(ostream &os) const {
  os << val;
}

MeasValue *MVFrequency::clone() const {
  return (new MVFrequency(*this));
}

Double MVFrequency::getValue() const {
  return val;
}

Quantity MVFrequency::get() const {
  return Quantity(val,"Hz");
}

Quantity MVFrequency::get(const Unit &unit) const {
  return Quantity(1.0/makeF(1.0/val, unit), unit);
}

Vector<Double> MVFrequency::getVector() const {
  Vector<Double> x(1);
  x(0) = val;
  return x;
}

void MVFrequency::putVector(const Vector<Double> &in) {
  if (in.nelements() < 1) {
    val = 0.0;
  } else {
    val = in(0);
  };
}

Double MVFrequency::makeF(Double v, const Unit &dt) const{
  static Bool needInit = True;
  static UnitVal InvTime;
  static UnitVal AngleTime;
  static UnitVal InvLength;
  static UnitVal Energy;
  static UnitVal Impuls;
  static Double LVel;
  static Double Planck;
  if (needInit) {
    needInit = False;
    InvTime = UnitVal::NODIM/UnitVal::TIME;
    AngleTime = UnitVal::ANGLE/UnitVal::TIME;
    InvLength = UnitVal::NODIM/UnitVal::LENGTH;
    Energy = UnitVal::MASS*UnitVal::LENGTH*UnitVal::LENGTH/
      UnitVal::TIME/UnitVal::TIME;
    Impuls = UnitVal::MASS*UnitVal::LENGTH;
    LVel = (QC::c).getBaseValue();
    Planck = (QC::h).getBaseValue();
  };
  if (dt.getValue() == UnitVal::TIME) {
    return (1.0/dt.getValue().getFac()/v);
  } else if (dt.getValue() == InvTime) {
    return (dt.getValue().getFac()*v);
  } else if (dt.getValue() == AngleTime) {
    return (dt.getValue().getFac()/C::pi/2.0*v);
  } else if (dt.getValue() == UnitVal::LENGTH) {
    return (LVel/dt.getValue().getFac()/v);
  } else if (dt.getValue() == InvLength) {
    return (LVel*dt.getValue().getFac()/C::pi/2.0*v);
  } else if (dt.getValue() == Energy) {
    return (dt.getValue().getFac()/Planck*v);
  } else {
    Quantity(1.0,dt).assert(Impuls);
    return (dt.getValue().getFac()*LVel/Planck*v);
  }
}
