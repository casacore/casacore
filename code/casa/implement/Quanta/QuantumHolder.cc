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
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Exceptions.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>

//# Constructors
QuantumHolder::QuantumHolder() : hold_p() {};

QuantumHolder::QuantumHolder(const QBase &in) :
  hold_p(in.clone()) {}

QuantumHolder::QuantumHolder(const QuantumHolder &other) : hold_p() {
  if (other.hold_p.ptr()) hold_p.set(other.hold_p.ptr()->clone());
}

//# Destructor
QuantumHolder::~QuantumHolder() {}

//# Operators
QuantumHolder &QuantumHolder::operator=(const QuantumHolder &other) {
  if (this != &other) hold_p.set(other.hold_p.ptr()->clone());
  return *this;
}

//# Member Functions
Bool QuantumHolder::isEmpty() const {
  return ToBool(!hold_p.ptr());
}

Bool QuantumHolder::isQuantum() const {
  return ToBool(hold_p.ptr());
}

Bool QuantumHolder::isScalar() const {
  return ToBool(hold_p.ptr() && nelements() == 1);
}

Bool QuantumHolder::isVector() const {
  return ToBool(hold_p.ptr() && ndim() == 1);
}

Bool QuantumHolder::isArray() const {
  return ToBool(hold_p.ptr() && ndim() > 0);
}

Bool QuantumHolder::isReal() const {
  return ToBool(hold_p.ptr() &&
		(isQuantumDouble() ||
		 isQuantumFloat() ||
		 isQuantumInt() ||
		 isQuantumArrayDouble() ||
		 isQuantumArrayFloat() ||
		 isQuantumArrayInt()));
}

Bool QuantumHolder::isComplex() const {
  return ToBool(hold_p.ptr() &&
		(isQuantumComplex() ||
		 isQuantumDComplex() ||
		 isQuantumArrayComplex() ||
		 isQuantumArrayDComplex()));
}

Bool QuantumHolder::isQuantity() const {
  return ToBool(hold_p.ptr() && isQuantumDouble());
}

Bool QuantumHolder::isQuantumDouble() const {
  return ToBool(hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<Double>::myType());
}

Bool QuantumHolder::isQuantumFloat() const {
  return ToBool(hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<Float>::myType());
}

Bool QuantumHolder::isQuantumInt() const {
  return ToBool(hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<Int>::myType());
}

Bool QuantumHolder::isQuantumComplex() const {
  return ToBool(hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<Complex>::myType());
}

Bool QuantumHolder::isQuantumDComplex() const {
  return ToBool(hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<DComplex>::myType());
}

Bool QuantumHolder::isQuantumArrayDouble() const {
  return ToBool(hold_p.ptr() &&
		(hold_p.ptr()->type() == Quantum<Array<Double> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<Double> >::myType()));
}

Bool QuantumHolder::isQuantumArrayFloat() const {
  return ToBool(hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<Float> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<Float> >::myType()));
}

Bool QuantumHolder::isQuantumArrayInt() const {
  return ToBool(hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<Int> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<Int> >::myType()));
}

Bool QuantumHolder::isQuantumArrayComplex() const {
  return ToBool(hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<Complex> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<Complex> >::myType()));
}

Bool QuantumHolder::isQuantumArrayDComplex() const {
  return ToBool(hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<DComplex> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<DComplex> >::myType()));
}

Bool QuantumHolder::isQuantumVectorDouble() const {
  return ToBool(isQuantumArrayDouble() && ndim() == 1);
}

Bool QuantumHolder::isQuantumVectorFloat() const {
  return ToBool(isQuantumArrayFloat() && ndim() == 1);
}

Bool QuantumHolder::isQuantumVectorInt() const {
  return ToBool(isQuantumArrayInt() && ndim() == 1);
}

Bool QuantumHolder::isQuantumVectorComplex() const {
  return ToBool(isQuantumArrayComplex() && ndim() == 1);
}

Bool QuantumHolder::isQuantumVectorDComplex() const {
  return ToBool(isQuantumArrayDComplex() && ndim() == 1);
}

Int QuantumHolder::nelements() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for nelements"));
  } else if (isQuantumVectorDouble()) {
    return ((Quantum<Vector<Double> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumVectorFloat()) {
    return ((Quantum<Vector<Float> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumVectorInt()) {
    return ((Quantum<Vector<Int> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumVectorComplex()) {
    return ((Quantum<Vector<Complex> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumVectorDComplex()) {
    return ((Quantum<Vector<DComplex> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayDouble()) {
    return ((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayFloat()) {
    return ((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayInt()) {
    return ((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayComplex()) {
    return ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayDComplex()) {
    return ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().nelements();
  };
  return 1;
}

Int QuantumHolder::ndim() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for ndim"));
  } else if (isQuantumVectorDouble()) {
    return ((Quantum<Vector<Double> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumVectorFloat()) {
    return ((Quantum<Vector<Float> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumVectorInt()) {
    return ((Quantum<Vector<Int> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumVectorComplex()) {
    return ((Quantum<Vector<Complex> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumVectorDComplex()) {
    return ((Quantum<Vector<DComplex> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayDouble()) {
    return ((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayFloat()) {
    return ((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayInt()) {
    return ((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayComplex()) {
    return ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayDComplex()) {
    return ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().ndim();
  };
  return 0;
}

const QBase &QuantumHolder::asQuantum() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantum"));
  };
  return *hold_p.ptr();
}

const Quantum<Double> &QuantumHolder::asQuantity() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDouble"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDouble"));
  };
  if (!isQuantity()) toReal(Quantum<Double>::myType());
  return (const Quantum<Double> &) *hold_p.ptr();
}

const Quantum<Double> &QuantumHolder::asQuantumDouble() {
  return asQuantity();
}

const Quantum<Float> &QuantumHolder::asQuantumFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumFloat"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumFloat"));
  };
  if (!isQuantumFloat()) toReal(Quantum<Float>::myType());
  return (const Quantum<Float> &) *hold_p.ptr();
}

const Quantum<Int> &QuantumHolder::asQuantumInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumInt"));
  };
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumInt"));
  };
  if (!isQuantumInt()) toReal(Quantum<Int>::myType());
  return (const Quantum<Int> &) *hold_p.ptr();
}

const Quantum<Complex> &QuantumHolder::asQuantumComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumComplex"));
  };
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumComplex"));
  };
  if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
  return (const Quantum<Complex> &) *hold_p.ptr();
}

const Quantum<DComplex> &QuantumHolder::asQuantumDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDComplex"));
  };
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDComplex"));
  };
  if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
  return (const Quantum<DComplex> &) *hold_p.ptr();
}

const Quantum<Vector<Double> > &QuantumHolder::asQuantumVectorDouble() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDouble"));
  };
  if (isArray()) {
    if (!isQuantumArrayDouble()) {
      throw(AipsError("Cannot convert to QuantumVectorDouble"));
    };
    if (ndim() != 1) {
      ((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorDouble"));
    };
    if (!isQuantumDouble()) toReal(Quantum<Double>::myType());
    toVector();
  };
  return (const Quantum<Vector<Double> > &) *hold_p.ptr();
}

const Quantum<Vector<Float> > &QuantumHolder::asQuantumVectorFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorFloat"));
  };
  if (isArray()) {
    if (!isQuantumArrayFloat()) {
      throw(AipsError("Cannot convert to QuantumVectorFloat"));
    };
    if (ndim() != 1) {
      ((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorFloat"));
    };
    if (!isQuantumFloat()) toReal(Quantum<Float>::myType());
    toVector();
  };
  return (const Quantum<Vector<Float> > &) *hold_p.ptr();
}

const Quantum<Vector<Int> > &QuantumHolder::asQuantumVectorInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorInt"));
  };
  if (isArray()) {
    if (!isQuantumArrayInt()) {
      throw(AipsError("Cannot convert to QuantumVectorInt"));
    };
    if (ndim() != 1) {
      ((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorInt"));
    };
    if (!isQuantumInt()) toReal(Quantum<Int>::myType());
    toVector();
  };
  return (const Quantum<Vector<Int> > &) *hold_p.ptr();
}

const Quantum<Vector<Complex> > &QuantumHolder::asQuantumVectorComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorComplex"));
  };
  if (isArray()) {
    if (!isQuantumArrayComplex()) {
      throw(AipsError("Cannot convert to QuantumVectorComplex"));
    };
    if (ndim() != 1) {
      ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    };
  } else {
    if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
    toVector();
  };
  return (const Quantum<Vector<Complex> > &) *hold_p.ptr();
}

const Quantum<Vector<DComplex> > &QuantumHolder::asQuantumVectorDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDComplex"));
  };
  if (isArray()) {
    if (!isQuantumArrayDComplex()) {
      throw(AipsError("Cannot convert to QuantumVectorDComplex"));
    };
    if (ndim() != 1) {
      ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    };
  } else {
    if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
    toVector();
  };
  return (const Quantum<Vector<DComplex> > &) *hold_p.ptr();
}

const Quantum<Array<Double> > &QuantumHolder::asQuantumArrayDouble() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayDouble"));
  };
  if (isArray()) {
    if (!isQuantumArrayDouble()) {
      throw(AipsError("Cannot convert to QuantumArrayDouble"));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayDouble"));
    };
    if (!isQuantumDouble()) toReal(Quantum<Double>::myType());
    toArray();
  };
  return (const Quantum<Array<Double> > &) *hold_p.ptr();
}

const Quantum<Array<Float> > &QuantumHolder::asQuantumArrayFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayFloat"));
  };
  if (isArray()) {
    if (!isQuantumArrayFloat()) {
      throw(AipsError("Cannot convert to QuantumArrayFloat"));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayFloat"));
    };
    if (!isQuantumFloat()) toReal(Quantum<Float>::myType());
    toArray();
  };
  return (const Quantum<Array<Float> > &) *hold_p.ptr();
}

const Quantum<Array<Int> > &QuantumHolder::asQuantumArrayInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayInt"));
  };
  if (isArray()) {
    if (!isQuantumArrayInt()) {
      throw(AipsError("Cannot convert to QuantumArrayInt"));
    };
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayInt"));
    };
    if (!isQuantumInt()) toReal(Quantum<Int>::myType());
    toArray();
  };
  return (const Quantum<Array<Int> > &) *hold_p.ptr();
}

const Quantum<Array<Complex> > &QuantumHolder::asQuantumArrayComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayComplex"));
  };
  if (isArray()) {
    if (!isQuantumArrayComplex()) {
      throw(AipsError("Cannot convert to QuantumArrayComplex"));
    };
  } else {
    if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
    toArray();
  };
  return (const Quantum<Array<Complex> > &) *hold_p.ptr();
}

const Quantum<Array<DComplex> > &QuantumHolder::asQuantumArrayDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayDComplex"));
  };
  if (isArray()) {
    if (!isQuantumArrayDComplex()) {
      throw(AipsError("Cannot convert to QuantumArrayDComplex"));
    };
  } else {
    if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
    toArray();
  };
  return (const Quantum<Array<DComplex> > &) *hold_p.ptr();
}

Bool QuantumHolder::fromRecord(String &error,
			       const RecordInterface &in) {
  String un;
  if (in.isDefined(String("value")) && in.isDefined(String("unit")) &&
      in.type(in.idToNumber(RecordFieldId("unit"))) == TpString) {
    String un;
    in.get(RecordFieldId("unit"), un);
    if (in.type(in.idToNumber(RecordFieldId("value"))) == TpDouble) {
      Double vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Double>(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) == TpFloat) {
      Float vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Float>(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) == TpInt) {
      Int vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Int>(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) == TpComplex) {
      Complex vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Complex>(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) == TpDComplex) {
      DComplex vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<DComplex>(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) ==
	       TpArrayDouble) {
      Array<Double> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<Double> >(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) ==
	       TpArrayFloat) {
      Array<Float> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<Float> >(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) ==
	       TpArrayInt) {
      Array<Int> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<Int> >(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) ==
	       TpArrayComplex) {
      Array<Complex> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<Complex> >(vl, un));
    } else if (in.type(in.idToNumber(RecordFieldId("value"))) ==
	       TpArrayDComplex) {
      Array<DComplex> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<DComplex> >(vl, un));
    } else {
      error +=
	String("Illegal Quantum datatype in QuantumHolder::fromRecord\n");
      return False;
    };
    return True;
  };
  error += String("Illegal Quantum record in QuantumHolder::fromRecord\n");
  return False;
}

Bool QuantumHolder::fromString(String &error,
			       const String &in) {
  Quantum<Double> res;
  if (!Quantum<Double>::read(res, in)) {
    error += String("Illegal input units or format in ") +
      String("QuantumHolder::fromString\n");
    return False;
  };
  hold_p.set(new Quantum<Double>(res));
  return True;
}

Bool QuantumHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold_p.ptr()) {
    if (out.isDefined("value")) out.removeField(RecordFieldId("value"));
    if (isQuantumDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Double> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumFloat()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<Float> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumInt()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<Int> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumComplex()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<Complex> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumDComplex()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<DComplex> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayFloat()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayInt()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayDComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<Double> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorFloat()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<Float> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorInt()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<Int> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<Complex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorDComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<DComplex> > *)(hold_p.ptr()))->getValue()));
    };
    out.define(RecordFieldId("unit"),
	       String(hold_p.ptr()->getFullUnit().getName()));
    return True;
  };
  error += String("No Quantum specified in QuantumHolder::toRecord\n");
  return False;
}

void QuantumHolder::toReal(const uInt &tp) {
  Double d1;
  if (isArray()) {
    IPosition stx(ndim(), 0);
    if (isQuantumArrayDouble()) {
      d1 = Double(((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayFloat()) {
      d1 = Double(((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayInt()) {
      d1 = Double(((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue()(stx));
    };
  } else {
    if (isQuantumDouble()) {
      d1 = Double(((Quantum<Double> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = Double(((Quantum<Float> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = Double(((Quantum<Int> *)(hold_p.ptr()))->getValue());
    };
  };
  Unit x = hold_p.ptr()->getFullUnit();
  if (tp == Quantum<Double>::myType()) {
    hold_p.set(new Quantum<Double>(d1, x));
  } else if (tp == Quantum<Float>::myType()) {
    hold_p.set(new Quantum<Float>(Float(d1), x));
  } else if (tp == Quantum<Int>::myType()) {
    hold_p.set(new Quantum<Int>(Int(d1), x));
  };
}

void QuantumHolder::toComplex(const uInt &tp) {
  DComplex d1;
  if (isArray()) {
    IPosition stx(ndim(), 0);
    if (isQuantumArrayDouble()) {
      d1 = DComplex(((Quantum<Array<Double> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayFloat()) {
      d1 = DComplex(((Quantum<Array<Float> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayInt()) {
      d1 = DComplex(((Quantum<Array<Int> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayComplex()) {
      d1 = (((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayDComplex()) {
      d1 = (((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue()(stx));
    };
  } else {
    if (isQuantumDouble()) {
      d1 = DComplex(((Quantum<Double> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = DComplex(((Quantum<Float> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = DComplex(((Quantum<Int> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumComplex()) {
      d1 = (((Quantum<Complex> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumDComplex()) {
      d1 = (((Quantum<DComplex> *)(hold_p.ptr()))->getValue());
    };
  };
  Unit x = hold_p.ptr()->getFullUnit();
  if (tp == Quantum<Complex>::myType()) {
    hold_p.set(new Quantum<Complex>(Complex(d1), x));
  } else  if (tp == Quantum<DComplex>::myType()) {
    hold_p.set(new Quantum<DComplex>(d1, x));
  };
}

void QuantumHolder::toVector() {
  Unit x = hold_p.ptr()->getFullUnit();
  if (isQuantumDouble()) {
    Vector<Double> d1(1);
    d1(0) = ((Quantum<Double> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<Double> >(d1, x));
  } else if (isQuantumFloat()) {
    Vector<Float> d1(1);
    d1(0) = ((Quantum<Float> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<Float> >(d1, x));
  } else if (isQuantumInt()) {
    Vector<Int> d1(1);
    d1(0) = ((Quantum<Int> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<Int> >(d1, x));
  } else if (isQuantumComplex()) {
    Vector<Complex> d1(1);
    d1(0) = ((Quantum<Complex> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<Complex> >(d1, x));
  } else if (isQuantumDComplex()) {
    Vector<DComplex> d1(1);
    d1(0) = ((Quantum<DComplex> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<DComplex> >(d1, x));
  };
}

void QuantumHolder::toArray() {
  toVector();
}

const String &QuantumHolder::ident() const {
  static String myid = "quant";
  return myid;
}
