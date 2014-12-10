//# MVFrequency.cc: Internal value for MFrequency
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/MVFrequency.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVFrequency class

//# Constructors
MVFrequency::MVFrequency() : 
  val(0.0){}

MVFrequency::MVFrequency(Double d) : 
  val(d){}

MVFrequency::MVFrequency(const MVFrequency &other) :
  MeasValue(),
  val(other.val)
{}

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
  }
}

MVFrequency::MVFrequency(const Vector<Double> &other) {
  uInt i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = other(0);
  } else {
    throw (AipsError("Illegal vector length in MVFrequency constructor"));
  }
}

MVFrequency::MVFrequency(const Vector<Quantity> &other) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantity vector in MVFrequency constructor"));
  }
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
  return (val == other.val);
}

Bool MVFrequency::operator!=(const MVFrequency &other) const {
  return (val != other.val);
}

Bool MVFrequency::near(const MVFrequency &other, Double tol) const {
  return ::casacore::near(val, other.val, tol);
}

Bool MVFrequency::nearAbs(const MVFrequency &other, Double tol) const {
  return ::casacore::nearAbs(val, other.val, tol);
}

// Member functions

uInt MVFrequency::type() const {
  return Register(static_cast<MVFrequency *>(0));
}

void MVFrequency::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVFrequency *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVFrequency"));
  }
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
  return Quantity(makeF(val, unit, True), unit);
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
  }
}

Vector<Quantum<Double> > MVFrequency::getRecordValue() const {
  Vector<Quantum<Double> > tmp(1);
  tmp(0) = get();
  return tmp;
}

Bool MVFrequency::putValue(const Vector<Quantum<Double> > &in) {
  static Bool needInit = True;
  static UnitVal InvTime;
  static UnitVal AngleTime;
  static UnitVal InvLength;
  static UnitVal Energy;
  static UnitVal Impuls;
  if (needInit) {
    needInit = False;
    InvTime = UnitVal::NODIM/UnitVal::TIME;
    AngleTime = UnitVal::ANGLE/UnitVal::TIME;
    InvLength = UnitVal::NODIM/UnitVal::LENGTH;
    Energy = UnitVal::MASS*UnitVal::LENGTH*UnitVal::LENGTH/
      UnitVal::TIME/UnitVal::TIME;
    Impuls = UnitVal::MASS*UnitVal::LENGTH;
  }
  uInt i = in.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    UnitVal dt = in(0).getFullUnit().getValue();
    if (dt == UnitVal::TIME ||
	dt == InvTime ||
	dt == AngleTime ||
	dt == UnitVal::LENGTH ||
	dt == InvLength ||
	dt == Energy ||
	dt == Impuls) {
      val = makeF(in(0).getValue(), in(0).getFullUnit());
    } else {
      return False;
    }
  } else {
    return False;
  }
  return True;
}

Double MVFrequency::makeF(Double v, const Unit &dt, Bool rev) const{
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
  }
  Double x;
  if (dt.getValue() == UnitVal::TIME) {
    return (1.0/dt.getValue().getFac()/v);
  } else if (dt.getValue() == InvTime) {
    x = dt.getValue().getFac();
  } else if (dt.getValue() == AngleTime) {
    x = dt.getValue().getFac()/C::pi/2.0;
  } else if (dt.getValue() == UnitVal::LENGTH) {
    return (LVel/dt.getValue().getFac()/v);
  } else if (dt.getValue() == InvLength) {
    x = LVel*dt.getValue().getFac()/C::pi/2.0;
  } else if (dt.getValue() == Energy) {
    x = dt.getValue().getFac()/Planck;
  } else {
    Quantity(1.0,dt).assure(Impuls);
    x = dt.getValue().getFac()*LVel/Planck;
  }
  if (rev) return (v/x);
  return (v*x);
}

} //# NAMESPACE CASACORE - END

