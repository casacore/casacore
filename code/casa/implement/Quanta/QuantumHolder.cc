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
  return ToBool(hold_.ptr() &&
  		hold_.ptr()->type() == Quantum<Vector<Float> >::myType());
}

Bool QuantumHolder::isQuantumVectorInt() const {
  return ToBool(hold_.ptr() &&
  		hold_.ptr()->type() == Quantum<Vector<Int> >::myType());
}

Bool QuantumHolder::isQuantumVectorComplex() const {
  return ToBool(hold_.ptr() &&
  		hold_.ptr()->type() == Quantum<Vector<Complex> >::myType());
}

Bool QuantumHolder::isQuantumVectorDComplex() const {
  return ToBool(hold_.ptr() &&
  		hold_.ptr()->type() == Quantum<Vector<DComplex> >::myType());
}

Int QuantumHolder::nelements() const {
  if (!hold_.ptr()) {
    return 0;
  } else if (isQuantumVectorDouble()) {
    return ((Quantum<Vector<Double> > *)(hold_.ptr()))->getValue().nelements();
  } else if (isQuantumVectorFloat()) {
    return ((Quantum<Vector<Float> > *)(hold_.ptr()))->getValue().nelements();
  } else if (isQuantumVectorInt()) {
    return ((Quantum<Vector<Int> > *)(hold_.ptr()))->getValue().nelements();
  } else if (isQuantumVectorComplex()) {
    return ((Quantum<Vector<Complex> > *)(hold_.ptr()))->getValue().nelements();
  } else if (isQuantumVectorDComplex()) {
    return ((Quantum<Vector<DComplex> > *)(hold_.ptr()))->getValue().nelements();
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
    throw(AipsError("Empty QuantumHolder argument for asQuantumFloat"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumFloat"));
  };
  if (!isQuantumFloat()) toReal(Quantum<Float>::myType());
  return (const Quantum<Float> &) *hold_.ptr();
}

const Quantum<Int> &QuantumHolder::asQuantumInt() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumInt"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumInt"));
  };
  if (!isQuantumInt()) toReal(Quantum<Int>::myType());
  return (const Quantum<Int> &) *hold_.ptr();
}

const Quantum<Complex> &QuantumHolder::asQuantumComplex() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumComplex"));
  };
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumComplex"));
  };
  if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
  return (const Quantum<Complex> &) *hold_.ptr();
}

const Quantum<DComplex> &QuantumHolder::asQuantumDComplex() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDComplex"));
  };
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDComplex"));
  };
  if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
  return (const Quantum<DComplex> &) *hold_.ptr();
}

const Quantum<Vector<Double> > &QuantumHolder::asQuantumVectorDouble() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDouble"));
  };
  if (!isQuantumVectorDouble()) {
    throw(AipsError("Cannot convert to QuantumVectorDouble"));
  };
  return (const Quantum<Vector<Double> > &) *hold_.ptr();
}

const Quantum<Vector<Float> > &QuantumHolder::asQuantumVectorFloat() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorFloat"));
  };
  if (!isQuantumVectorFloat()) {
    throw(AipsError("Cannot convert to QuantumVectorFloat"));
  };
  return (const Quantum<Vector<Float> > &) *hold_.ptr();
}

const Quantum<Vector<Int> > &QuantumHolder::asQuantumVectorInt() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorInt"));
  };
  if (!isQuantumVectorInt()) {
    throw(AipsError("Cannot convert to QuantumVectorInt"));
  };
  return (const Quantum<Vector<Int> > &) *hold_.ptr();
}

const Quantum<Vector<Complex> > &QuantumHolder::asQuantumVectorComplex() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorComplex"));
  };
  if (!isQuantumVectorComplex()) {
    throw(AipsError("Cannot convert to QuantumVectorComplex"));
  };
  return (const Quantum<Vector<Complex> > &) *hold_.ptr();
}

const Quantum<Vector<DComplex> > &QuantumHolder::asQuantumVectorDComplex() {
  if (!hold_.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDComplex"));
  };
  if (!isQuantumVectorDComplex()) {
    throw(AipsError("Cannot convert to QuantumVectorDComplex"));
  };
  return (const Quantum<Vector<DComplex> > &) *hold_.ptr();
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

void QuantumHolder::toReal(const uInt &tp) {
  Double d1;
  if (isArray()) {
    if (isQuantumVectorDouble()) {
      d1 = Double(((Quantum<Vector<Double> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorFloat()) {
      d1 = Double(((Quantum<Vector<Float> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorInt()) {
      d1 = Double(((Quantum<Vector<Int> > *)(hold_.ptr()))->getValue()(0));
    };
  } else {
    if (isQuantumDouble()) {
      d1 = Double(((Quantum<Double> *)(hold_.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = Double(((Quantum<Float> *)(hold_.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = Double(((Quantum<Int> *)(hold_.ptr()))->getValue());
    };
  };
  Unit x = hold_.ptr()->getFullUnit();
  if (tp == Quantum<Double>::myType()) {
    hold_.set(new Quantum<Double>(d1, x));
  } else if (tp == Quantum<Float>::myType()) {
    hold_.set(new Quantum<Float>(Float(d1), x));
  } else if (tp == Quantum<Int>::myType()) {
    hold_.set(new Quantum<Int>(Int(d1), x));
  };
}

void QuantumHolder::toComplex(const uInt &tp) {
  DComplex d1;
  if (isArray()) {
    if (isQuantumVectorDouble()) {
      d1 = DComplex(((Quantum<Vector<Double> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorFloat()) {
      d1 = DComplex(((Quantum<Vector<Float> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorInt()) {
      d1 = DComplex(((Quantum<Vector<Int> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorComplex()) {
      d1 = (((Quantum<Vector<DComplex> > *)(hold_.ptr()))->getValue()(0));
    } else if (isQuantumVectorComplex()) {
      d1 = (((Quantum<Vector<DComplex> > *)(hold_.ptr()))->getValue()(0));
    };
  } else {
    if (isQuantumDouble()) {
      d1 = DComplex(((Quantum<Double> *)(hold_.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = DComplex(((Quantum<Float> *)(hold_.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = DComplex(((Quantum<Int> *)(hold_.ptr()))->getValue());
    } else if (isQuantumComplex()) {
      d1 = (((Quantum<Complex> *)(hold_.ptr()))->getValue());
    } else if (isQuantumDComplex()) {
      d1 = (((Quantum<DComplex> *)(hold_.ptr()))->getValue());
    };
  };
  Unit x = hold_.ptr()->getFullUnit();
  if (tp == Quantum<Complex>::myType()) {
    hold_.set(new Quantum<Complex>(Complex(d1), x));
  } else  if (tp == Quantum<DComplex>::myType()) {
    hold_.set(new Quantum<DComplex>(d1, x));
  };
}

void QuantumHolder::toVector() {
}
