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
//#
//# $Id$

//# Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/MVDouble.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVDouble class

//# Constructors
MVDouble::MVDouble(Double d) : 
  val(d){}

MVDouble::MVDouble(const MVDouble &other) :
  MeasValue(),
  val(other.val)
{}

MVDouble::MVDouble(const Quantity &other) {
  val = other.get().getValue();
}

MVDouble::MVDouble(const Quantum<Vector<Double> > &other) {
  Vector<Double> tmp;
  tmp = other.get().getValue();
  uInt i = tmp.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = tmp(0);
  } else {
    throw (AipsError("Illegal vector length in MVDouble constructor"));
  }
}

MVDouble::MVDouble(const Vector<Double> &other) {
  uInt i = other.nelements();
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
MVDouble::operator Double() const {
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

Bool MVDouble::operator==(const MVDouble &other) const {
  return (val == other.val);
}

Bool MVDouble::operator!=(const MVDouble &other) const {
  return (val != other.val);
}

//# Member functions

uInt MVDouble::type() const {
  return Register(static_cast<MVDouble *>(0));
}

void MVDouble::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVDouble *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVDouble"));
  }
}

Bool MVDouble::near(const MVDouble &other, Double tol) const {
  return ::casacore::near(val, other.val, tol);
}

Bool MVDouble::nearAbs(const MVDouble &other, Double tol) const {
  return ::casacore::nearAbs(val, other.val, tol);
}

// Member functions
void MVDouble::print(ostream &os) const {
  os << val;
}

MeasValue *MVDouble::clone() const {
  return (new MVDouble(*this));
}

Vector<Double> MVDouble::getVector() const {
  Vector<Double> x(1);
  x(0) = val;
  return x;
}

void MVDouble::putVector(const Vector<Double> &in) {
  if (in.nelements() < 1) {
    val = 0.0;
  } else {
    val = in(0);
  }
}

Vector<Quantum<Double> > MVDouble::getRecordValue() const {
  Vector<Quantum<Double> > tmp(1);
  tmp(0) = Quantity(val, "");
  return tmp;
}

Bool MVDouble::putValue(const Vector<Quantum<Double> > &in) {
  uInt i = in.nelements();
  if (i == 0) {
    val = 0.0;
  } else if (i == 1) {
    val = (in(0)).get().getValue();
  } else {
    return False;
  }
  return True;
}

} //# NAMESPACE CASACORE - END

