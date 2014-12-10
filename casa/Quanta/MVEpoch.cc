//# MVEpoch.cc: a class for high precision time
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double MVEpoch::secInDay(3600*24);

//# Constructors
MVEpoch::MVEpoch() :
  wday(0), frday(0) {}

MVEpoch::MVEpoch(const MVEpoch &other) :
  MeasValue(),
  wday (other.wday),
  frday (other.frday)
{
  adjust();
}

MVEpoch::MVEpoch(Double inday, Double infrac) :
  wday(0), frday(0) {
  addTime(inday);
  addTime(infrac);
  adjust();
}

MVEpoch::MVEpoch(const Quantity &in) : 
  wday(0), frday(0) {
  addTime(makeDay(in));
}

MVEpoch::MVEpoch(const Quantity &in1, const Quantity &in2) : 
  wday(0), frday(0) {
  addTime(makeDay(in1));
  addTime(makeDay(in2));
  adjust();
}

MVEpoch::MVEpoch(const Quantum<Vector<Double> > &in) : 
  wday(0), frday(0) {
  for (uInt i=0; i<in.getValue().nelements(); i++) {
    addTime(makeDay(Quantity((in.getValue())(i),in.getUnit())));
  }
  adjust();
}

MVEpoch::MVEpoch(const Vector<Double> &inday) :
  wday(0), frday(0) {
  for (uInt i=0; i<inday.nelements(); i++) addTime(inday(i));
  adjust();
}

MVEpoch::MVEpoch(const Vector<Quantity> &in) :
  wday(0), frday(0) {
  if (!putValue(in)) {
    throw(AipsError("Illegal Quantity type argument: MVEpoch"));
  }
}

//# Destructor
MVEpoch::~MVEpoch() {}

//# Operators
MVEpoch &MVEpoch::operator=(const MVEpoch &other) {
  if (this != &other) {
    wday = other.wday;
    frday = other.frday;
  }
  return *this;
}

MVEpoch &MVEpoch::operator+=(const MVEpoch &other) {
  frday += other.frday;
  wday += other.wday;
  adjust();
  return *this;
}

MVEpoch MVEpoch::operator+(const MVEpoch &other) const {
  MVEpoch tmp(*this);
  tmp += other;
  return tmp;
}

MVEpoch &MVEpoch::operator-=(const MVEpoch &other) {
  frday -= other.frday;
  wday -= other.wday;
  adjust();
  return *this;
}

MVEpoch MVEpoch::operator-(const MVEpoch &other) const {
  MVEpoch tmp = *this;
  tmp -= other;
  return tmp;
}

Bool MVEpoch::operator==(const MVEpoch &other) const {
  return (wday == other.wday && frday == other.frday);
}

Bool MVEpoch::operator!=(const MVEpoch &other) const {
  return (!( *this == other));
}

Bool MVEpoch::near(const MVEpoch &other, Double tol) const {
  return ::casacore::near(get(), other.get(), tol);
}

Bool MVEpoch::nearAbs(const MVEpoch &other, Double tol) const {
  return ::casacore::nearAbs(get(), other.get(), tol);
}

//# Member functions

uInt MVEpoch::type() const {
  return Register(static_cast<MVEpoch *>(0));
}

void MVEpoch::assure(const MeasValue &in) {
  if (in.type() != Register(static_cast<MVEpoch *>(0))) {
    throw(AipsError("Illegal MeasValue type argument: MVEpoch"));
  }
}

void MVEpoch::adjust() {
  while (frday < 0) {
    frday += 1; wday -= 1;
  }
  while (frday >= 1) {
    frday -= 1; wday += 1;
  }
}

void MVEpoch::adjust(Double &res) {
  adjust();
  res = 1.0;
}

Double MVEpoch::get() const {
  return (wday + frday);
}

Quantity MVEpoch::getTime() const {
  return (Quantity(get(), "d"));
}

Quantity MVEpoch::getTime(const Unit &unit) const {
  return (getTime().get(unit));
}

Double MVEpoch::getDay() const {
  return wday;
}

Double MVEpoch::getDayFraction() const {
  return frday;
}

void MVEpoch::print(ostream &os) const {
  Int h = ifloor(24.*frday);
  Int m = ifloor(60.*(24.*frday - h));
  Double s = MVEpoch::secInDay*frday - m*60. - h*3600.;
  Int prec = os.precision();
  Char fill = os.fill();
  os << getDay() << "::";
  os << setfill('0') << setw(2) << h << ":" <<
    setw(2) << m << ':' << 
    setprecision(max(prec-2,2));
  ios::fmtflags oldb = os.setf(ios::fixed,ios::floatfield);
  os << setw(os.precision()+3) << s <<
    setprecision(prec);
  os.setf(oldb,ios::floatfield);
  os.fill(fill);
}

MeasValue *MVEpoch::clone() const {
  return (new MVEpoch(*this));
}

Vector<Double> MVEpoch::getVector() const {
  Vector<Double> x(2);
  x(0) = wday;
  x(1) = frday;
  return x;
}

void MVEpoch::putVector(const Vector<Double> &in) {
  if (in.nelements() < 2) {
    wday = 0.0; frday = 0.0;
    if (in.nelements() == 1) addTime(in(0));
  } else {
    wday = in(0);
    frday = in(1);
  }
}

Vector<Quantum<Double> > MVEpoch::getRecordValue() const {
  Vector<Quantum<Double> > tmp(1);
  tmp(0) = getTime();
  return tmp;
}

Bool MVEpoch::putValue(const Vector<Quantum<Double> > &in) {
  for (uInt i=0; i<in.nelements(); i++) {
    if (!in(i).check(UnitVal::TIME)) return False;
  }
  wday = frday = 0;
  for (uInt i=0; i<in.nelements(); i++) addTime(makeDay(in(i)));
  adjust();
  return True;
}

Double MVEpoch::makeDay(const Quantity &in) const {
  in.assure(UnitVal::TIME);
  return in.get("d").getValue();
}

void MVEpoch::addTime(Double in) {
  Double t = std::floor(in);
  wday += t;
  frday += (in-t);
}

} //# NAMESPACE CASACORE - END

