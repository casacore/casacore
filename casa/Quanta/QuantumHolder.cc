//# QuantumHolder.cc: A holder for Quantum to enable record conversions
//# Copyright (C) 1998,1999,2000,2001
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
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
QuantumHolder::QuantumHolder() 
  : hold_p() {}

QuantumHolder::QuantumHolder(const QBase &in) 
  : hold_p(in.clone()) {}

QuantumHolder::QuantumHolder(const QuantumHolder &other) 
  : RecordTransformable(),
    hold_p()
{
  if (other.hold_p.ptr()) hold_p.set(other.hold_p.ptr()->clone());
}

//# Destructor
QuantumHolder::~QuantumHolder() {}

//# Operators
QuantumHolder &QuantumHolder::operator=(const QuantumHolder &other) {
  if (this != &other) {
    if (other.hold_p.ptr()) {
      hold_p.set(other.hold_p.ptr()->clone());
    } else {
      hold_p.clear();
    }
  }
  return *this;
}

//# Member Functions
bool QuantumHolder::isEmpty() const {
  return (!hold_p.ptr());
}

bool QuantumHolder::isQuantum() const {
  return (hold_p.ptr());
}

bool QuantumHolder::isScalar() const {
  return (hold_p.ptr() && nelements() == 1);
}

bool QuantumHolder::isVector() const {
  return (hold_p.ptr() && ndim() == 1);
}

bool QuantumHolder::isArray() const {
  return (hold_p.ptr() && ndim() > 0);
}

bool QuantumHolder::isReal() const {
  return (hold_p.ptr() &&
		(isQuantumDouble() ||
		 isQuantumFloat() ||
		 isQuantumInt() ||
		 isQuantumArrayDouble() ||
		 isQuantumArrayFloat() ||
		 isQuantumArrayInt()));
}

bool QuantumHolder::isComplex() const {
  return (hold_p.ptr() &&
		(isQuantumComplex() ||
		 isQuantumDComplex() ||
		 isQuantumArrayComplex() ||
		 isQuantumArrayDComplex()));
}

bool QuantumHolder::isQuantity() const {
  return (hold_p.ptr() && isQuantumDouble());
}

bool QuantumHolder::isQuantumDouble() const {
  return (hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<double>::myType());
}

bool QuantumHolder::isQuantumFloat() const {
  return (hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<float>::myType());
}

bool QuantumHolder::isQuantumInt() const {
  return (hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<int32_t>::myType());
}

bool QuantumHolder::isQuantumComplex() const {
  return (hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<Complex>::myType());
}

bool QuantumHolder::isQuantumDComplex() const {
  return (hold_p.ptr() &&
		hold_p.ptr()->type() == Quantum<DComplex>::myType());
}

bool QuantumHolder::isQuantumArrayDouble() const {
  return (hold_p.ptr() &&
		(hold_p.ptr()->type() == Quantum<Array<double> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<double> >::myType()));
}

bool QuantumHolder::isQuantumArrayFloat() const {
  return (hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<float> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<float> >::myType()));
}

bool QuantumHolder::isQuantumArrayInt() const {
  return (hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<int32_t> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<int32_t> >::myType()));
}

bool QuantumHolder::isQuantumArrayComplex() const {
  return (hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<Complex> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<Complex> >::myType()));
}

bool QuantumHolder::isQuantumArrayDComplex() const {
  return (hold_p.ptr() &&
  		(hold_p.ptr()->type() == Quantum<Array<DComplex> >::myType() ||
		 hold_p.ptr()->type() == Quantum<Vector<DComplex> >::myType()));
}

bool QuantumHolder::isQuantumVectorDouble() const {
  return (isQuantumArrayDouble() && ndim() == 1);
}

bool QuantumHolder::isQuantumVectorFloat() const {
  return (isQuantumArrayFloat() && ndim() == 1);
}

bool QuantumHolder::isQuantumVectorInt() const {
  return (isQuantumArrayInt() && ndim() == 1);
}

bool QuantumHolder::isQuantumVectorComplex() const {
  return (isQuantumArrayComplex() && ndim() == 1);
}

bool QuantumHolder::isQuantumVectorDComplex() const {
  return (isQuantumArrayDComplex() && ndim() == 1);
}

int32_t QuantumHolder::nelements() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for nelements"));
  } else if (isQuantumArrayDouble()) {
    return ((Quantum<Array<double> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayFloat()) {
    return ((Quantum<Array<float> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayInt()) {
    return ((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayComplex()) {
    return ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().nelements();
  } else if (isQuantumArrayDComplex()) {
    return ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().nelements();
  }
  return 1;
}

int32_t QuantumHolder::ndim() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for ndim"));
  } else if (isQuantumArrayDouble()) {
    return ((Quantum<Array<double> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayFloat()) {
    return ((Quantum<Array<float> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayInt()) {
    return ((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayComplex()) {
    return ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().ndim();
  } else if (isQuantumArrayDComplex()) {
    return ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().ndim();
  }
  return 0;
}

const QBase &QuantumHolder::asQuantum() const {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantum"));
  }
  return *hold_p.ptr();
}

const Quantum<double> &QuantumHolder::asQuantity() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDouble"));
  }
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDouble"));
  }
  if (!isQuantity()) toReal(Quantum<double>::myType());
  return (const Quantum<double> &) *hold_p.ptr();
}

const Quantum<double> &QuantumHolder::asQuantumDouble() {
  return asQuantity();
}

const Quantum<float> &QuantumHolder::asQuantumFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumFloat"));
  }
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumFloat"));
  }
  if (!isQuantumFloat()) toReal(Quantum<float>::myType());
  return (const Quantum<float> &) *hold_p.ptr();
}

const Quantum<int32_t> &QuantumHolder::asQuantumInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumInt"));
  }
  if (!isReal() || !isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumInt"));
  }
  if (!isQuantumInt()) toReal(Quantum<int32_t>::myType());
  return (const Quantum<int32_t> &) *hold_p.ptr();
}

const Quantum<Complex> &QuantumHolder::asQuantumComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumComplex"));
  }
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumComplex"));
  }
  if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
  return (const Quantum<Complex> &) *hold_p.ptr();
}

const Quantum<DComplex> &QuantumHolder::asQuantumDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumDComplex"));
  }
  if (!isScalar()) {
    throw(AipsError("Wrong QuantumHolder to convert asQuantumDComplex"));
  }
  if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
  return (const Quantum<DComplex> &) *hold_p.ptr();
}

const Quantum<Vector<double> > &QuantumHolder::asQuantumVectorDouble() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDouble"));
  }
  if (isArray()) {
    if (!isQuantumArrayDouble()) {
      throw(AipsError("Cannot convert to QuantumVectorDouble"));
    }
    if (ndim() != 1) {
      ((Quantum<Array<double> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorDouble"));
    }
    if (!isQuantumDouble()) toReal(Quantum<double>::myType());
    toVector();
  }
  return (const Quantum<Vector<double> > &) *hold_p.ptr();
}

const Quantum<Vector<float> > &QuantumHolder::asQuantumVectorFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorFloat"));
  }
  if (isArray()) {
    if (!isQuantumArrayFloat()) {
      throw(AipsError("Cannot convert to QuantumVectorFloat"));
    }
    if (ndim() != 1) {
      ((Quantum<Array<float> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorFloat"));
    }
    if (!isQuantumFloat()) toReal(Quantum<float>::myType());
    toVector();
  }
  return (const Quantum<Vector<float> > &) *hold_p.ptr();
}

const Quantum<Vector<int32_t> > &QuantumHolder::asQuantumVectorInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorInt"));
  }
  if (isArray()) {
    if (!isQuantumArrayInt()) {
      throw(AipsError("Cannot convert to QuantumVectorInt"));
    }
    if (ndim() != 1) {
      ((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumVectorInt"));
    }
    if (!isQuantumInt()) toReal(Quantum<int32_t>::myType());
    toVector();
  }
  return (const Quantum<Vector<int32_t> > &) *hold_p.ptr();
}

const Quantum<Vector<Complex> > &QuantumHolder::asQuantumVectorComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorComplex"));
  }
  if (isArray()) {
    if (!isQuantumArrayComplex()) {
      throw(AipsError("Cannot convert to QuantumVectorComplex"));
    }
    if (ndim() != 1) {
      ((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    }
  } else {
    if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
    toVector();
  }
  return (const Quantum<Vector<Complex> > &) *hold_p.ptr();
}

const Quantum<Vector<DComplex> > &QuantumHolder::asQuantumVectorDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumVectorDComplex"));
  }
  if (isArray()) {
    if (!isQuantumArrayDComplex()) {
      throw(AipsError("Cannot convert to QuantumVectorDComplex"));
    }
    if (ndim() != 1) {
      ((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue().
	reform(IPosition(1, nelements()));
    }
  } else {
    if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
    toVector();
  }
  return (const Quantum<Vector<DComplex> > &) *hold_p.ptr();
}

const Quantum<Array<double> > &QuantumHolder::asQuantumArrayDouble() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayDouble"));
  }
  if (isArray()) {
    if (!isQuantumArrayDouble()) {
      throw(AipsError("Cannot convert to QuantumArrayDouble"));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayDouble"));
    }
    if (!isQuantumDouble()) toReal(Quantum<double>::myType());
    toArray();
  }
  return (const Quantum<Array<double> > &) *hold_p.ptr();
}

const Quantum<Array<float> > &QuantumHolder::asQuantumArrayFloat() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayFloat"));
  }
  if (isArray()) {
    if (!isQuantumArrayFloat()) {
      throw(AipsError("Cannot convert to QuantumArrayFloat"));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayFloat"));
    }
    if (!isQuantumFloat()) toReal(Quantum<float>::myType());
    toArray();
  }
  return (const Quantum<Array<float> > &) *hold_p.ptr();
}

const Quantum<Array<int32_t> > &QuantumHolder::asQuantumArrayInt() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayInt"));
  }
  if (isArray()) {
    if (!isQuantumArrayInt()) {
      throw(AipsError("Cannot convert to QuantumArrayInt"));
    }
  } else {
    if (!isReal()) {
      throw(AipsError("Wrong QuantumHolder to convert asQuantumArrayInt"));
    }
    if (!isQuantumInt()) toReal(Quantum<int32_t>::myType());
    toArray();
  }
  return (const Quantum<Array<int32_t> > &) *hold_p.ptr();
}

const Quantum<Array<Complex> > &QuantumHolder::asQuantumArrayComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayComplex"));
  }
  if (isArray()) {
    if (!isQuantumArrayComplex()) {
      throw(AipsError("Cannot convert to QuantumArrayComplex"));
    }
  } else {
    if (!isQuantumComplex()) toComplex(Quantum<Complex>::myType());
    toArray();
  }
  return (const Quantum<Array<Complex> > &) *hold_p.ptr();
}

const Quantum<Array<DComplex> > &QuantumHolder::asQuantumArrayDComplex() {
  if (!hold_p.ptr()) {
    throw(AipsError("Empty QuantumHolder argument for asQuantumArrayDComplex"));
  }
  if (isArray()) {
    if (!isQuantumArrayDComplex()) {
      throw(AipsError("Cannot convert to QuantumArrayDComplex"));
    }
  } else {
    if (!isQuantumDComplex()) toComplex(Quantum<DComplex>::myType());
    toArray();
  }
  return (const Quantum<Array<DComplex> > &) *hold_p.ptr();
}

bool QuantumHolder::fromRecord(String &error,
			       const RecordInterface &in) {
  String un;
  if (in.isDefined(String("value")) && in.isDefined(String("unit")) &&
      in.type(in.idToNumber(RecordFieldId("unit"))) == TpString) {
    String un;
    in.get(RecordFieldId("unit"), un);
    switch (in.type(in.idToNumber(RecordFieldId("value")))) {
    case TpDouble:
    {
      double vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<double>(vl, un));
      return true;
    }
    case TpFloat:
    {
      float vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<float>(vl, un));
      return true;
    }
    case TpInt:
    {
      int32_t vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<int32_t>(vl, un));
      return true;
    }
    case TpComplex:
    {
      Complex vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Complex>(vl, un));
      return true;
    }
    case TpDComplex:
    {
      DComplex vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<DComplex>(vl, un));
      return true;
    }
    case TpArrayDouble:
    {
      Array<double> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<double> >(vl, un));
      return true;
    }
    case TpArrayFloat:
    {
      Array<float> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<float> >(vl, un));
      return true;
    }
    case TpArrayInt:
    {
      Array<int32_t> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<int32_t> >(vl, un));
      return true;
    }
    case TpArrayComplex:
    {
      Array<Complex> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<Complex> >(vl, un));
      return true;
    }
    case TpArrayDComplex:
    {
      Array<DComplex> vl;
      in.get(RecordFieldId("value"), vl);
      hold_p.set(new Quantum<Array<DComplex> >(vl, un));
      return true;
    }
    default:
      break;
    }
  }
  error += String("Illegal Quantum record in QuantumHolder::fromRecord\n");
  return false;
}

bool QuantumHolder::fromString(String &error,
			       const String &in) {
  Quantum<double> res;
  if (!Quantum<double>::read(res, in)) {
    error += String("in QuantumHolder::fromString with input string \"") +
      in + String("\": Illegal input units or format\n");
    return false;
  }
  hold_p.set(new Quantum<double>(res));
  return true;
}

bool QuantumHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold_p.ptr()) {
    if (out.isDefined("value")) out.removeField(RecordFieldId("value"));
    if (isQuantumDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<double> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumFloat()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<float> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumInt()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<int32_t> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumComplex()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<Complex> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumDComplex()) {
      out.define(RecordFieldId("value"),
                 (((Quantum<DComplex> *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<double> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorFloat()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<float> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorInt()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<int32_t> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<Complex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumVectorDComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Vector<DComplex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayDouble()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<double> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayFloat()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<float> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayInt()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue()));
    } else if (isQuantumArrayDComplex()) {
      out.define(RecordFieldId("value"),
		 (((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue()));
    }
    out.define(RecordFieldId("unit"),
	       String(hold_p.ptr()->getFullUnit().getName()));
    return true;
  }
  error += String("No Quantum specified in QuantumHolder::toRecord\n");
  return false;
}

void QuantumHolder::toRecord(RecordInterface &out) const {
	String error;
	if (! toRecord(error, out)) {
		throw AipsError(error);
	}
}

Record QuantumHolder::toRecord() const {
	Record r;
	toRecord(r);
	return r;
}


void QuantumHolder::toReal(const uint32_t &tp) {
  double d1=0;
  if (isArray()) {
    IPosition stx(ndim(), 0);
    if (isQuantumArrayDouble()) {
      d1 = double(((Quantum<Array<double> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayFloat()) {
      d1 = double(((Quantum<Array<float> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayInt()) {
      d1 = double(((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue()(stx));
    }
  } else {
    if (isQuantumDouble()) {
      d1 = double(((Quantum<double> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = double(((Quantum<float> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = double(((Quantum<int32_t> *)(hold_p.ptr()))->getValue());
    }
  }
  Unit x = hold_p.ptr()->getFullUnit();
  if (tp == Quantum<double>::myType()) {
    hold_p.set(new Quantum<double>(d1, x));
  } else if (tp == Quantum<float>::myType()) {
    hold_p.set(new Quantum<float>(float(d1), x));
  } else if (tp == Quantum<int32_t>::myType()) {
    hold_p.set(new Quantum<int32_t>(int32_t(d1), x));
  }
}

void QuantumHolder::toComplex(const uint32_t &tp) {
  DComplex d1;
  if (isArray()) {
    IPosition stx(ndim(), 0);
    if (isQuantumArrayDouble()) {
      d1 = DComplex(((Quantum<Array<double> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayFloat()) {
      d1 = DComplex(((Quantum<Array<float> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayInt()) {
      d1 = DComplex(((Quantum<Array<int32_t> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayComplex()) {
      d1 = (((Quantum<Array<Complex> > *)(hold_p.ptr()))->getValue()(stx));
    } else if (isQuantumArrayDComplex()) {
      d1 = (((Quantum<Array<DComplex> > *)(hold_p.ptr()))->getValue()(stx));
    }
  } else {
    if (isQuantumDouble()) {
      d1 = DComplex(((Quantum<double> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumFloat()) {
      d1 = DComplex(((Quantum<float> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumInt()) {
      d1 = DComplex(((Quantum<int32_t> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumComplex()) {
      d1 = (((Quantum<Complex> *)(hold_p.ptr()))->getValue());
    } else if (isQuantumDComplex()) {
      d1 = (((Quantum<DComplex> *)(hold_p.ptr()))->getValue());
    }
  }
  Unit x = hold_p.ptr()->getFullUnit();
  if (tp == Quantum<Complex>::myType()) {
    hold_p.set(new Quantum<Complex>(Complex(d1), x));
  } else  if (tp == Quantum<DComplex>::myType()) {
    hold_p.set(new Quantum<DComplex>(d1, x));
  }
}

void QuantumHolder::toVector() {
  Unit x = hold_p.ptr()->getFullUnit();
  if (isQuantumDouble()) {
    Vector<double> d1(1);
    d1(0) = ((Quantum<double> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<double> >(d1, x));
  } else if (isQuantumFloat()) {
    Vector<float> d1(1);
    d1(0) = ((Quantum<float> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<float> >(d1, x));
  } else if (isQuantumInt()) {
    Vector<int32_t> d1(1);
    d1(0) = ((Quantum<int32_t> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<int32_t> >(d1, x));
  } else if (isQuantumComplex()) {
    Vector<Complex> d1(1);
    d1(0) = ((Quantum<Complex> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<Complex> >(d1, x));
  } else if (isQuantumDComplex()) {
    Vector<DComplex> d1(1);
    d1(0) = ((Quantum<DComplex> *)(hold_p.ptr()))->getValue();
    hold_p.set(new Quantum<Vector<DComplex> >(d1, x));
  }
}

void QuantumHolder::toArray() {
  toVector();
}

const String &QuantumHolder::ident() const {
  static const String myid = "quant";
  return myid;
}

} //# NAMESPACE CASACORE - END

