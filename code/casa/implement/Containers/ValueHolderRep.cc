//# ValueHolderRep.cc: A holder object for the standard AIPS++ data
//# Copyright (C) 2005
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
//#
//# $Id$


#include <casa/Containers/ValueHolderRep.h>
#include <casa/Containers/Record.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN


ValueHolderRep::ValueHolderRep (Bool value)
  : itsCount(0),
    itsType (TpBool),
    itsBool (value)
{}

ValueHolderRep::ValueHolderRep (uChar value)
  : itsCount(0),
    itsType  (TpUChar),
    itsUChar (value)
{}

ValueHolderRep::ValueHolderRep (Short value)
  : itsCount(0),
    itsType  (TpShort),
    itsShort (value)
{}

ValueHolderRep::ValueHolderRep (uShort value)
  : itsCount(0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (Int value)
  : itsCount(0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (uInt value)
  : itsCount(0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (Float value)
  : itsCount(0),
    itsType  (TpFloat),
    itsFloat (value)
{}

ValueHolderRep::ValueHolderRep (Double value)
  : itsCount(0),
    itsType   (TpDouble),
    itsDouble (value)
{}

ValueHolderRep::ValueHolderRep (const Complex& value)
  : itsCount(0),
    itsType (TpComplex),
    itsPtr  (new Complex(value))
{}

ValueHolderRep::ValueHolderRep (const DComplex& value)
  : itsCount(0),
    itsType (TpDComplex),
    itsPtr  (new DComplex(value))
{}

ValueHolderRep::ValueHolderRep (const String& value)
  : itsCount(0),
    itsType (TpString),
    itsPtr  (new String(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Bool>& value)
  : itsCount(0),
    itsType (TpArrayBool),
    itsPtr  (new Array<Bool>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uChar>& value)
  : itsCount(0),
    itsType (TpArrayUChar),
    itsPtr  (new Array<uChar>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Short>& value)
  : itsCount(0),
    itsType (TpArrayShort),
    itsPtr  (new Array<Short>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uShort>& value)
  : itsCount(0),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray (*static_cast<Array<Int>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<Int>& value)
  : itsCount(0),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uInt>& value)
  : itsCount(0),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray (*static_cast<Array<Int>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<Float>& value)
  : itsCount(0),
    itsType (TpArrayFloat),
    itsPtr  (new Array<Float>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Double>& value)
  : itsCount(0),
    itsType (TpArrayDouble),
    itsPtr  (new Array<Double>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Complex>& value)
  : itsCount(0),
    itsType (TpArrayComplex),
    itsPtr  (new Array<Complex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<DComplex>& value)
  : itsCount(0),
    itsType (TpArrayDComplex),
    itsPtr  (new Array<DComplex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<String>& value)
  : itsCount(0),
    itsType (TpArrayString),
    itsPtr  (new Array<String>(value))
{}

ValueHolderRep::ValueHolderRep (const Record& value)
  : itsCount(0),
    itsType (TpRecord),
    itsPtr  (new Record(value))
{}

ValueHolderRep::~ValueHolderRep()
{
  switch (itsType) {
  case TpBool:
  case TpUChar:
  case TpShort:
  case TpFloat:
  case TpDouble:
    break;
  case TpComplex:
    delete static_cast<Complex*>(itsPtr);
    break;
  case TpDComplex:
    delete static_cast<DComplex*>(itsPtr);
    break;
  case TpString:
    delete static_cast<String*>(itsPtr);
    break;
  case TpArrayBool:
    delete static_cast<Array<Bool>*>(itsPtr);
    break;
  case TpArrayUChar:
    delete static_cast<Array<uChar>*>(itsPtr);
    break;
  case TpArrayShort:
    delete static_cast<Array<Short>*>(itsPtr);
    break;
  case TpArrayInt:
    delete static_cast<Array<Int>*>(itsPtr);
    break;
  case TpArrayFloat:
    delete static_cast<Array<Float>*>(itsPtr);
    break;
  case TpArrayDouble:
    delete static_cast<Array<Double>*>(itsPtr);
    break;
  case TpArrayComplex:
    delete static_cast<Array<Complex>*>(itsPtr);
    break;
  case TpArrayDComplex:
    delete static_cast<Array<DComplex>*>(itsPtr);
    break;
  case TpArrayString:
    delete static_cast<Array<String>*>(itsPtr);
    break;
  case TpRecord:
    delete static_cast<Record*>(itsPtr);
    break;
  default:
    break;
  }
}

Bool ValueHolderRep::asBool() const
{
  AlwaysAssert (itsType == TpBool, AipsError);
  return itsBool;
}

uChar ValueHolderRep::asuChar() const
{
  AlwaysAssert (itsType == TpUChar, AipsError);
  return itsUChar;
}

Short ValueHolderRep::asShort() const
{
  AlwaysAssert (itsType == TpShort, AipsError);
  return itsShort;
}

uShort ValueHolderRep::asuShort() const
{
  AlwaysAssert (itsType == TpInt, AipsError);
  return uShort(itsInt);
}

Int ValueHolderRep::asInt() const
{
  AlwaysAssert (itsType == TpInt, AipsError);
  return itsInt;
}

uInt ValueHolderRep::asuInt() const
{
  AlwaysAssert (itsType == TpInt, AipsError);
  return uInt(itsInt);
}

Float ValueHolderRep::asFloat() const
{
  AlwaysAssert (itsType == TpFloat, AipsError);
  return itsFloat;
}

Double ValueHolderRep::asDouble() const
{
  AlwaysAssert (itsType == TpDouble, AipsError);
  return itsDouble;
}

const Complex& ValueHolderRep::asComplex() const
{
  AlwaysAssert (itsType == TpComplex, AipsError);
  return *static_cast<Complex*>(itsPtr);
}

const DComplex& ValueHolderRep::asDComplex() const
{
  AlwaysAssert (itsType == TpDComplex, AipsError);
  return *static_cast<DComplex*>(itsPtr);
}

const String& ValueHolderRep::asString() const
{
  AlwaysAssert (itsType == TpString, AipsError);
  return *static_cast<String*>(itsPtr);
}

const Array<Bool>& ValueHolderRep::asArrayBool() const
{
  AlwaysAssert (itsType == TpArrayBool, AipsError);
  return *static_cast<Array<Bool>*>(itsPtr);
}

const Array<uChar>& ValueHolderRep::asArrayuChar() const
{
  AlwaysAssert (itsType == TpArrayUChar, AipsError);
  return *static_cast<Array<uChar>*>(itsPtr);
}

const Array<Short>& ValueHolderRep::asArrayShort() const
{
  AlwaysAssert (itsType == TpArrayShort, AipsError);
  return *static_cast<Array<Short>*>(itsPtr);
}

const Array<uShort> ValueHolderRep::asArrayuShort() const
{
  AlwaysAssert (itsType == TpArrayInt, AipsError);
  const Array<Int>* in = static_cast<Array<Int>*>(itsPtr);
  Array<uShort> out(in->shape());
  convertArray (out, *in);
  return out;
}

const Array<Int>& ValueHolderRep::asArrayInt() const
{
  AlwaysAssert (itsType == TpArrayInt, AipsError);
  return *static_cast<Array<Int>*>(itsPtr);
}

const Array<uInt> ValueHolderRep::asArrayuInt() const
{
  AlwaysAssert (itsType == TpArrayInt, AipsError);
  const Array<Int>* in = static_cast<Array<Int>*>(itsPtr);
  Array<uInt> out(in->shape());
  convertArray (out, *in);
  return out;
}

const Array<Float>& ValueHolderRep::asArrayFloat() const
{
  AlwaysAssert (itsType == TpArrayFloat, AipsError);
  return *static_cast<Array<Float>*>(itsPtr);
}

const Array<Double>& ValueHolderRep::asArrayDouble() const
{
  AlwaysAssert (itsType == TpArrayDouble, AipsError);
  return *static_cast<Array<Double>*>(itsPtr);
}

const Array<Complex>& ValueHolderRep::asArrayComplex() const
{
  AlwaysAssert (itsType == TpArrayComplex, AipsError);
  return *static_cast<Array<Complex>*>(itsPtr);
}

const Array<DComplex>& ValueHolderRep::asArrayDComplex() const
{
  AlwaysAssert (itsType == TpArrayDComplex, AipsError);
  return *static_cast<Array<DComplex>*>(itsPtr);
}

const Array<String>& ValueHolderRep::asArrayString() const
{
  AlwaysAssert (itsType == TpArrayString, AipsError);
  return *static_cast<Array<String>*>(itsPtr);
}

const Record& ValueHolderRep::asRecord() const
{
  AlwaysAssert (itsType == TpRecord, AipsError);
  return *static_cast<Record*>(itsPtr);
}


void ValueHolderRep::toRecord (Record& rec, const RecordFieldId& id) const
{
  switch (itsType) {
  case TpBool:
    rec.define (id, itsBool);
    break;
  case TpUChar:
    rec.define (id, itsUChar);
    break;
  case TpShort:
    rec.define (id, itsShort);
    break;
  case TpInt:
    rec.define (id, itsInt);
    break;
  case TpFloat:
    rec.define (id, itsFloat);
    break;
  case TpDouble:
    rec.define (id, itsDouble);
    break;
  case TpComplex:
    rec.define (id, *static_cast<Complex*>(itsPtr));
    break;
  case TpDComplex:
    rec.define (id, *static_cast<DComplex*>(itsPtr));
    break;
  case TpString:
    rec.define (id, *static_cast<String*>(itsPtr));
    break;
  case TpArrayBool:
    rec.define (id, *static_cast<Array<Bool>*>(itsPtr));
    break;
  case TpArrayUChar:
    rec.define (id, *static_cast<Array<uChar>*>(itsPtr));
    break;
  case TpArrayShort:
    rec.define (id, *static_cast<Array<Short>*>(itsPtr));
    break;
  case TpArrayInt:
    rec.define (id, *static_cast<Array<Int>*>(itsPtr));
    break;
  case TpArrayFloat:
    rec.define (id, *static_cast<Array<Float>*>(itsPtr));
    break;
  case TpArrayDouble:
    rec.define (id, *static_cast<Array<Double>*>(itsPtr));
    break;
  case TpArrayComplex:
    rec.define (id, *static_cast<Array<Complex>*>(itsPtr));
    break;
  case TpArrayDComplex:
    rec.define (id, *static_cast<Array<DComplex>*>(itsPtr));
    break;
  case TpArrayString:
    rec.define (id, *static_cast<Array<String>*>(itsPtr));
    break;
  case TpRecord:
    rec.defineRecord (id, *static_cast<Record*>(itsPtr));
    break;
  default:
    break;
  }
}

  // Construct the object from the value in a record.
ValueHolderRep* ValueHolderRep::fromRecord (const Record& rec,
					    const RecordFieldId& id)
{
  switch (rec.dataType(id)) {
  case TpBool:
    return new ValueHolderRep (rec.asBool(id));
  case TpUChar:
    return new ValueHolderRep (rec.asuChar(id));
  case TpShort:
    return new ValueHolderRep (rec.asShort(id));
  case TpInt:
    return new ValueHolderRep (rec.asInt(id));
  case TpFloat:
    return new ValueHolderRep (rec.asFloat(id));
  case TpDouble:
    return new ValueHolderRep (rec.asDouble(id));
  case TpComplex:
    return new ValueHolderRep (rec.asComplex(id));
  case TpDComplex:
    return new ValueHolderRep (rec.asDComplex(id));
  case TpString:
    return new ValueHolderRep (rec.asString(id));
  case TpArrayBool:
    return new ValueHolderRep (rec.asArrayBool(id));
  case TpArrayUChar:
    return new ValueHolderRep (rec.asArrayuChar(id));
  case TpArrayShort:
    return new ValueHolderRep (rec.asArrayShort(id));
  case TpArrayInt:
    return new ValueHolderRep (rec.asArrayInt(id));
  case TpArrayFloat:
    return new ValueHolderRep (rec.asArrayFloat(id));
  case TpArrayDouble:
    return new ValueHolderRep (rec.asArrayDouble(id));
  case TpArrayComplex:
    return new ValueHolderRep (rec.asArrayComplex(id));
  case TpArrayDComplex:
    return new ValueHolderRep (rec.asArrayDComplex(id));
  case TpArrayString:
    return new ValueHolderRep (rec.asArrayString(id));
  case TpRecord:
    return new ValueHolderRep (rec.subRecord(id));
  default:
    break;
  }
  throw AipsError ("ValueHolder::fromRecord - unknown data type");
}

} //# NAMESPACE CASA - END
