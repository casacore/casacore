//# MVDoppler.cc: Internal value for MDoppler
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/MVDoppler.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVDoppler class

//# Constructors
MVDoppler::MVDoppler() : 
  val(0.0){}

MVDoppler::MVDoppler(double d) : 
  val(d){}

MVDoppler::MVDoppler(const MVDoppler &other) :
  MeasValue(),
  val(other.val)
{}

MVDoppler::MVDoppler(const Quantity &other) {
  val = makeD(other.getValue(), other.getFullUnit());
}

MVDoppler::MVDoppler(const Quantum<Vector<double> > &other) {
  Vector<double> tmp;
  tmp = other.getValue();
  uint32_t i = tmp.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = makeD(tmp(0), other.getFullUnit());
  } else {
    throw (AipsError("Illegal vector length in MVDoppler constructor"));
  }
}

MVDoppler::MVDoppler(const Vector<double> &other) {
  uint32_t i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = other(0);
  } else {
    throw (AipsError("Illegal vector length in MVDoppler constructor"));
  }
}

MVDoppler::MVDoppler(const Vector<Quantity> &other) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantity vector in MVDoppler constructor"));
  }
}

MVDoppler &MVDoppler::operator=(const MVDoppler &other) {
  if (this != &other) {
    val = other.val;
  }
  return *this;
}

// Destructor
MVDoppler::~MVDoppler() {}

// Operators
MVDoppler::operator double() const {
  return val;
}

MVDoppler &MVDoppler::operator+=(const MVDoppler &other) {
  val += other.val;
  return *this;
}

MVDoppler &MVDoppler::operator-=(const MVDoppler &other) {
  val -= other.val;
  return *this;
}

bool MVDoppler::operator==(const MVDoppler &other) const {
  return (val == other.val);
}

bool MVDoppler::operator!=(const MVDoppler &other) const {
  return (val != other.val);
}

bool MVDoppler::near(const MVDoppler &other, double tol) const {
  return ::casacore::near(val, other.val, tol);
}

bool MVDoppler::nearAbs(const MVDoppler &other, double tol) const {
  return ::casacore::nearAbs(val, other.val, tol);
}

// Member functions

void MVDoppler::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVDoppler*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVDoppler"));
  }
}

void MVDoppler::print(ostream &os) const {
  os << val;
}

MeasValue *MVDoppler::clone() const {
  return (new MVDoppler(*this));
}

double MVDoppler::getValue() const {
  return val;
}

Quantity MVDoppler::get() const {
  return Quantity(val*C::c,"m/s");
}

Quantity MVDoppler::get(const Unit &unit) const {
  return Quantity(makeD(val, unit, true), unit);
}

Vector<double> MVDoppler::getVector() const {
  Vector<double> x(1);
  x(0) = val;
  return x;
}

void MVDoppler::putVector(const Vector<double> &in) {
  if (in.nelements() < 1) {
    val = 0.0;
  } else {
    val = in(0);
  }
}

Vector<Quantum<double> > MVDoppler::getRecordValue() const {
  Vector<Quantum<double> > tmp(1);
  tmp(0) = get();
  return tmp;
}

bool MVDoppler::putValue(const Vector<Quantum<double> > &in) {
  static const UnitVal Velocity = UnitVal::LENGTH/UnitVal::TIME;
  uint32_t i = in.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    UnitVal dt = in(0).getFullUnit().getValue();
    if (dt == UnitVal::NODIM ||
	dt == Velocity) {
      val = makeD(in(0).getValue(), in(0).getFullUnit());
    } else {
      return false;
    }
  } else {
    return false;
  }
  return true;
}

double MVDoppler::makeD(double v, const Unit &dt, bool rev) const{
  static const UnitVal Velocity = UnitVal::LENGTH/UnitVal::TIME;
  static const double LVel = QC::c( ).getBaseValue();
  double x;
  if (dt.getValue() == UnitVal::NODIM) {
    x = dt.getValue().getFac();
  } else {
    Quantity(1.0,dt).assure(Velocity);
    x = dt.getValue().getFac()/LVel;
  }
  if (rev) return (v/x);
  return (v*x);
}

} //# NAMESPACE CASACORE - END

