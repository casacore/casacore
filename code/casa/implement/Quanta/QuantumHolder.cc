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
#include <aips/Arrays/Vector.h>

//# Constructors
QuantumHolder::QuantumHolder() : hold_() {};

QuantumHolder::QuantumHolder(const QBase &in) :
  hold_(in.clone()) {}

QuantumHolder::QuantumHolder(const QuantumHolder &other) : hold_() {
  if (other.hold_.ptr()) hold_.set(other.hold_.ptr()->clone());
}

//# Destructor
QuantumHolder::~QuantumHolder() {}

//# Operators
QuantumHolder &QuantumHolder::operator=(const QuantumHolder &other) {
  if (this != &other) hold_.set(other.hold_.ptr()->clone());
  return *this;
}

const QBase &QuantumHolder::operator()() const {
  return *hold_.ptr();
}

//# Member Functions
Bool QuantumHolder::isEmpty() const {
  return ToBool(!hold_.ptr());
}

Bool QuantumHolder::isQuantum() const {
  return ToBool(hold_.ptr());
}

Bool QuantumHolder::isScalar() const {
  return ToBool(hold_.ptr() &&
		nelements() == 1);
}

Bool QuantumHolder::isArray() const {
  return ToBool(hold_.ptr() &&
		(isQuantumVectorDouble() ||
		isQuantumVectorFloat() ||
		isQuantumVectorInt() ||
		isQuantumVectorComplex() ||
		isQuantumVectorDComplex()));
}

Bool QuantumHolder::isReal() const {
  return ToBool(hold_.ptr() &&
		(isQuantumDouble() ||
		isQuantumFloat() ||
		isQuantumInt() ||
		isQuantumVectorDouble() ||
		isQuantumVectorFloat() ||
		isQuantumVectorInt()));
}

Bool QuantumHolder::isComplex() const {
  return ToBool(hold_.ptr() &&
		(isQuantumComplex() ||
		 isQuantumDComplex() ||
		 isQuantumVectorComplex() ||
		 isQuantumVectorDComplex()));
}

Bool QuantumHolder::isQuantity() const {
  return ToBool(hold_.ptr() &&
		isQuantumDouble());
}

Bool QuantumHolder::isQuantumDouble() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<Double>::myType());
}

Bool QuantumHolder::isQuantumFloat() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<Float>::myType());
}

Bool QuantumHolder::isQuantumInt() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<Int>::myType());
}

Bool QuantumHolder::isQuantumComplex() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<Complex>::myType());
}

Bool QuantumHolder::isQuantumDComplex() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<DComplex>::myType());
}

Bool QuantumHolder::isQuantumVectorDouble() const {
  return ToBool(hold_.ptr() &&
		hold_.ptr()->type() == Quantum<Vector<Double> >::myType());
}

Bool QuantumHolder::isQuantumVectorFloat() const {
  ///  return ToBool(hold_.ptr() &&
  ///		hold_.ptr()->type() == Quantum<Vector<Float> >::myType());
  return False; ///
}

Bool QuantumHolder::isQuantumVectorInt() const {
  ///  return ToBool(hold_.ptr() &&
  ///		hold_.ptr()->type() == Quantum<Vector<Int> >::myType());
  return False;///
}

Bool QuantumHolder::isQuantumVectorComplex() const {
  ///  return ToBool(hold_.ptr() &&
  ///		hold_.ptr()->type() == Quantum<Vector<Complex> >::myType());
  return False;///
}

Bool QuantumHolder::isQuantumVectorDComplex() const {
  ///  return ToBool(hold_.ptr() &&
  ///		hold_.ptr()->type() == Quantum<Vector<DComplex> >::myType());
  return False;///
}

Int QuantumHolder::nelements() const {
  if (!hold_.ptr()) {
    return 0;
  } else if (isQuantumVectorDouble()) {
    return ((Quantum<Vector<Double> > *)(hold_.ptr()))->getValue().nelements();
    ///  } else if (isQuantumVectorFloat()) {
    ///    return ((Quantum<Vector<Float> > *)(hold_.ptr()))->getValue().nelements();
    ///  } else if (isQuantumVectorInt()) {
    ///    return ((Quantum<Vector<Int> > *)(hold_.ptr()))->getValue().nelements();
    ///  } else if (isQuantumVectorComplex()) {
    ///    return ((Quantum<Vector<Complex> > *)(hold_.ptr()))->getValue().nelements();
    ///  } else if (isQuantumVectorDComplex()) {
    ///    return ((Quantum<Vector<DComplex> > *)(hold_.ptr()))->getValue().nelements();
  };
  return 1;
}

const QBase &QuantumHolder::asQuantum() const {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantum"));
  };
  return *hold_.ptr();
}

const Quantum<Double> &QuantumHolder::asQuantity() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDouble"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDouble"));
  };
  if (!isQuantity()) toReal(Quantum<Double>::myType());
  return (const Quantum<Double> &) *hold_.ptr();
}

const Quantum<Double> &QuantumHolder::asQuantumDouble() {
  return asQuantity();
}

const Quantum<Float> &QuantumHolder::asQuantumFloat() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantity"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumFloat"));
  };
  if (!isQuantumFloat()) toReal(Quantum<Float>::myType());
  return (const Quantum<Float> &) *hold_.ptr();
}

const Quantum<Int> &QuantumHolder::asQuantumInt() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantity"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumInt"));
  };
  if (!isQuantumInt()) toReal(Quantum<Int>::myType());
  return (const Quantum<Int> &) *hold_.ptr();
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
    hold_.set(new Quantum<Double>(vl, un));
    return True;
  };
  error = String("Illegal Quantity record in QuantumHolder::fromRecord\n") +
    error;
  return False;
}

Bool QuantumHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold_.ptr()) {
    if (hold_.ptr()->type() == Quantum<Double>::myType()) {
      out.define(RecordFieldId("value"),
		 Double(((Quantum<Double> *)(hold_.ptr()))->getValue()));
    } else if (hold_.ptr()->type() == Quantum<Float>::myType()) { 
      out.define(RecordFieldId("value"),
                 Double(((Quantum<Float> *)(hold_.ptr()))->getValue()));
    } else if (hold_.ptr()->type() == Quantum<Int>::myType()) {
      out.define(RecordFieldId("value"),
                 Double(((Quantum<Int> *)(hold_.ptr()))->getValue()));
    };
    out.define(RecordFieldId("unit"),
	       String(hold_.ptr()->getFullUnit().getName()));
    return True;
  };
  error = String("No Quantity specified in QuantumHolder::toRecord\n") +
    error;
  return False;
}

void QuantumHolder::toReal(const Int &tp) {
}
