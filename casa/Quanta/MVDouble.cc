//# MVDouble.cc: to disticguish between internal and external Measure values
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
#include <casacore/casa/Quanta/MVDouble.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVDouble class

//# Constructors
MVDouble::MVDouble(double d) : 
  val(d){}

MVDouble::MVDouble(const MVDouble &other) :
  MeasValue(),
  val(other.val)
{}

MVDouble::MVDouble(const Quantity &other) {
  val = other.get().getValue();
}

MVDouble::MVDouble(const Quantum<Vector<double> > &other) {
  Vector<double> tmp;
  tmp = other.get().getValue();
  uint32_t i = tmp.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = tmp(0);
  } else {
    throw (AipsError("Illegal vector length in MVDouble constructor"));
  }
}

MVDouble::MVDouble(const Vector<double> &other) {
  uint32_t i = other.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = other(0);
  } else {
    throw (AipsError("Illegal vector length in MVDouble constructor"));
  }
}

MVDouble::MVDouble(const Vector<Quantity> &other) {
  if (!putValue(other)) {
    throw (AipsError("Illegal quantity vector in MVDouble constructor"));
  }
}

MVDouble &MVDouble::operator=(const MVDouble &other) {
  if (this != &other) {
    val = other.val;
  }
  return *this;
}

// Destructor
MVDouble::~MVDouble() {}

// Operators
MVDouble::operator double() const {
  return val;
}

MVDouble &MVDouble::operator+=(const MVDouble &other) {
  val += other.val;
  return *this;
}

MVDouble &MVDouble::operator-=(const MVDouble &other) {
  val -= other.val;
  return *this;
}

bool MVDouble::operator==(const MVDouble &other) const {
  return (val == other.val);
}

bool MVDouble::operator!=(const MVDouble &other) const {
  return (val != other.val);
}

//# Member functions

void MVDouble::assure(const MeasValue &in) {
  if (!dynamic_cast<const MVDouble *>(&in)) {
    throw(AipsError("Illegal MeasValue type argument: MVDouble"));
  }
}

bool MVDouble::near(const MVDouble &other, double tol) const {
  return ::casacore::near(val, other.val, tol);
}

bool MVDouble::nearAbs(const MVDouble &other, double tol) const {
  return ::casacore::nearAbs(val, other.val, tol);
}

// Member functions
void MVDouble::print(ostream &os) const {
  os << val;
}

MeasValue *MVDouble::clone() const {
  return (new MVDouble(*this));
}

Vector<double> MVDouble::getVector() const {
  Vector<double> x(1);
  x(0) = val;
  return x;
}

void MVDouble::putVector(const Vector<double> &in) {
  if (in.nelements() < 1) {
    val = 0.0;
  } else {
    val = in(0);
  }
}

Vector<Quantum<double> > MVDouble::getRecordValue() const {
  Vector<Quantum<double> > tmp(1);
  tmp(0) = Quantity(val, "");
  return tmp;
}

bool MVDouble::putValue(const Vector<Quantum<double> > &in) {
  uint32_t i = in.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = (in(0)).get().getValue();
  } else {
    return false;
  }
  return true;
}

} //# NAMESPACE CASACORE - END

