//# QuantumHolder.cc: A holder for Quantum to enable record conversions
//# Copyright (C) 1998
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
#include <trial/Measures/QuantumHolder.h>
#include <aips/Exceptions.h>
#include <aips/Measures/UnitVal.h>
#include <aips/Measures/Quantum.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/String.h>

//# Constructors
QuantumHolder::QuantumHolder() : hold() {};

QuantumHolder::QuantumHolder(const Quantum<Double> &in) :
  hold(new Quantum<Double>(in)) {}

QuantumHolder::QuantumHolder(const Quantum<Float> &in) :
  hold(new Quantum<Double>(in.getValue(), in.getFullUnit().getName())) {}

QuantumHolder::QuantumHolder(const Quantum<Int> &in) :
  hold(new Quantum<Double>(in.getValue(), in.getFullUnit().getName())) {}

QuantumHolder::QuantumHolder(const QuantumHolder &other) : hold() {
  if (other.hold.ptr()) hold.set(new Quantum<Double>(*other.hold.ptr()));
}

//# Destructor
QuantumHolder::~QuantumHolder() {}

//# Operators
QuantumHolder &QuantumHolder::operator=(const QuantumHolder &other) {
  if (this != &other) hold.set(new Quantum<Double>(*other.hold.ptr()));
  return *this;
}

const Quantum<Double> &QuantumHolder::operator()() const {
  return *hold.ptr();
}

//# Member Functions
Bool QuantumHolder::isQuantity() const {
  return ToBool(hold.ptr());
}

Bool QuantumHolder::isEmpty() const {
  return ToBool(!hold.ptr());
}

const Quantum<Double> &QuantumHolder::asQuantity() const {
  if (!hold.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantity"));
  };
  return *hold.ptr();
}

Bool QuantumHolder::fromRecord(String &error,
				const RecordInterface &in) {
  String un;
  if (in.isDefined(String("value")) && in.isDefined(String("unit")) &&
      (in.type(in.idToNumber(RecordFieldId("value"))) == TpDouble ||
       in.type(in.idToNumber(RecordFieldId("value"))) == TpFloat ||
       in.type(in.idToNumber(RecordFieldId("value"))) == TpInt) &&
      in.type(in.idToNumber(RecordFieldId("unit"))) == TpString &&
      (in.get(RecordFieldId("unit"), un), UnitVal::check(un))) {
    Double vl;
    String un;
    in.get(RecordFieldId("value"), vl);
    in.get(RecordFieldId("unit"), un);
    hold.set(new Quantum<Double>(vl, un));
    return True;
  };
  error = String("Illegal Quantity record in QuantumHolder::fromRecord\n") +
    error;
  return False;
}

Bool QuantumHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold.ptr()) {
    out.define(RecordFieldId("value"),
	       Double(hold.ptr()->getValue()));
    out.define(RecordFieldId("unit"),
	       String(hold.ptr()->getFullUnit().getName()));
    return True;
  };
  error = String("No Quantity specified in QuantumHolder::toRecord\n") +
    error	;
  return False;
}
