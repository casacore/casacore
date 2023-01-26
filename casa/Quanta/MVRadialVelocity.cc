//# MVRadialVelocity.cc: Internal value for MRadialvelocity
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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
#include <casacore/casa/Quanta/MVRadialVelocity.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/MVFrequency.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVRadialVelocity class

//# Constructors
MVRadialVelocity::MVRadialVelocity() : 
  val(0.0){}

MVRadialVelocity::MVRadialVelocity(double d) : 
  val(d){}

MVRadialVelocity::MVRadialVelocity(const MVRadialVelocity &other) :
  MeasValue(),
  val(other.val)
{}

MVRadialVelocity::MVRadialVelocity(const Quantity &other) {
  val = other.getValue() * makeF(other.getFullUnit());
}

MVRadialVelocity::MVRadialVelocity(const Quantum<Vector<double> > &other) {
  Vector<double> tmp;
  tmp = other.getValue();
  uint32_t i = tmp.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = tmp(0) * makeF(other.getFullUnit());
  } else {
    throw (AipsError("Illegal vector length in MVRadialVelocity constructor"));
  }
}

MVRadialVelocity::MVRadialVelocity(const Vector<double> &other) {
  uint32_t i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = other(0);
  } else {
    throw (AipsError("Illegal vector length in MVRadialVelocity constructor"));
  }
}

MVRadialVelocity::MVRadialVelocity(const Vector<Quantity> &other) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantity vector in MVRadialVelocity constructor"));
  }
}

MVRadialVelocity &MVRadialVelocity::operator=(const MVRadialVelocity &other) {
  if (this != &other) {
    val = other.val;
  }
  return *this;
}

// Destructor
MVRadialVelocity::~MVRadialVelocity() {}

// Operators
MVRadialVelocity::operator double() const {
  return val;
}

MVRadialVelocity &MVRadialVelocity::operator+=(const MVRadialVelocity &other) {
  val += other.val;
  return *this;
}

MVRadialVelocity &MVRadialVelocity::operator-=(const MVRadialVelocity &other) {
  val -= other.val;
  return *this;
}

bool MVRadialVelocity::operator==(const MVRadialVelocity &other) const {
  return (val == other.val);
}

bool MVRadialVelocity::operator!=(const MVRadialVelocity &other) const {
  return (val != other.val);
}

bool MVRadialVelocity::near(const MVRadialVelocity &other, double tol) const {
  return ::casacore::near(val, other.val, tol);
}

bool MVRadialVelocity::nearAbs(const MVRadialVelocity &other, double tol) const {
  return ::casacore::nearAbs(val, other.val, tol);
}

// Member functions

void MVRadialVelocity::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVRadialVelocity*>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVRadialVelocity"));
  }
}

void MVRadialVelocity::print(ostream &os) const {
  os << val;
}

MeasValue *MVRadialVelocity::clone() const {
  return (new MVRadialVelocity(*this));
}

double MVRadialVelocity::getValue() const {
  return val;
}

Quantity MVRadialVelocity::get() const {
  return Quantity(val,"m/s");
}

Quantity MVRadialVelocity::get(const Unit &unit) const {
  return Quantity(val/makeF(unit), unit);
}

Vector<double> MVRadialVelocity::getVector() const {
  Vector<double> x(1);
  x(0) = val;
  return x;
}

void MVRadialVelocity::putVector(const Vector<double> &in) {
  if (in.nelements() < 1) {
    val = 0.0;
  } else {
    val = in(0);
  }
}

Vector<Quantum<double> > MVRadialVelocity::getRecordValue() const {
  Vector<Quantum<double> > tmp(1);
  tmp(0) = get();
  return tmp;
}

bool MVRadialVelocity::putValue(const Vector<Quantum<double> > &in) {
  static const UnitVal Velocity = UnitVal::LENGTH/UnitVal::TIME;
  uint32_t i = in.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    UnitVal dt = in(0).getFullUnit().getValue();
    if (dt == Velocity) {
      val = in(0).getValue() * makeF(in(0).getFullUnit());
    } else {
      return false;
    }
  } else {
    return false;
  }
  return true;
}

Vector<double>
MVRadialVelocity::shiftFrequency(const Vector<double> &freq) const {
  Vector<double> tmp(freq.nelements());
  double factor = val/C::c;
  factor = sqrt((1-factor)/(1+factor));
  for (uint32_t i=0; i<freq.nelements(); ++i) tmp[i] = freq[i] * factor;
  return tmp;
}

Quantum<Vector<double> >
MVRadialVelocity::shiftFrequency(const Quantum<Vector<double> > &freq) const {
  Vector<double> tmp(freq.getValue().nelements());
  tmp = freq.getValue();
  double factor = val/C::c;
  factor = sqrt((1-factor)/(1+factor));
  for (uint32_t i=0; i<tmp.nelements(); ++i) {
    tmp[i] = MVFrequency(Quantity(tmp[i],freq.getFullUnit())).getValue() *
			 factor;
  }
  for (uint32_t i=0; i<tmp.nelements(); ++i) {
    tmp[i] = MVFrequency(tmp[i]).get(freq.getFullUnit()).getValue();
  }
  return Quantum<Vector<double> >(tmp, freq.getFullUnit());
}

double MVRadialVelocity::makeF(const Unit &dt) const{
  static const UnitVal Velocity = UnitVal::LENGTH/UnitVal::TIME;
  Quantity(1.0,dt).assure(Velocity);
  return (dt.getValue().getFac());
}

} //# NAMESPACE CASACORE - END

