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
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN


ValueHolderRep::ValueHolderRep (Bool value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpBool),
    itsBool (value)
{}

ValueHolderRep::ValueHolderRep (uChar value)
  : itsCount(0),
    itsNdim (0),
    itsType  (TpUChar),
    itsUChar (value)
{}

ValueHolderRep::ValueHolderRep (Short value)
  : itsCount(0),
    itsNdim (0),
    itsType  (TpShort),
    itsShort (value)
{}

ValueHolderRep::ValueHolderRep (uShort value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (Int value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (uInt value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpInt),
    itsInt  (value)
{}

ValueHolderRep::ValueHolderRep (Float value)
  : itsCount(0),
    itsNdim (0),
    itsType  (TpFloat),
    itsFloat (value)
{}

ValueHolderRep::ValueHolderRep (Double value)
  : itsCount(0),
    itsNdim (0),
    itsType   (TpDouble),
    itsDouble (value)
{}

ValueHolderRep::ValueHolderRep (const Complex& value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpComplex),
    itsPtr  (new Complex(value))
{}

ValueHolderRep::ValueHolderRep (const DComplex& value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpDComplex),
    itsPtr  (new DComplex(value))
{}

ValueHolderRep::ValueHolderRep (const String& value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpString),
    itsPtr  (new String(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Bool>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayBool),
    itsPtr  (new Array<Bool>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uChar>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayUChar),
    itsPtr  (new Array<uChar>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Short>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayShort),
    itsPtr  (new Array<Short>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uShort>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray (*static_cast<Array<Int>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<Int>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uInt>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray (*static_cast<Array<Int>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<Float>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayFloat),
    itsPtr  (new Array<Float>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Double>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayDouble),
    itsPtr  (new Array<Double>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Complex>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayComplex),
    itsPtr  (new Array<Complex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<DComplex>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayDComplex),
    itsPtr  (new Array<DComplex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<String>& value)
  : itsCount(0),
    itsNdim (value.ndim()),
    itsType (TpArrayString),
    itsPtr  (new Array<String>(value))
{}

ValueHolderRep::ValueHolderRep (const Record& value)
  : itsCount(0),
    itsNdim (0),
    itsType (TpRecord),
    itsPtr  (new Record(value))
{}

  ValueHolderRep::ValueHolderRep (uInt ndim, Bool)
  : itsCount(0),
    itsNdim (ndim),
    itsType (TpOther),
    itsPtr  (0)
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
  switch (itsType) {
  case TpBool:
    return itsBool;
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return itsFloat;
  case TpDouble:
    return itsDouble;
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asBool - invalid data type");
}

uChar ValueHolderRep::asuChar() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    AlwaysAssert (itsShort >= 0  &&  itsShort < 256, AipsError);
    return itsShort;
  case TpInt:
    AlwaysAssert (itsInt >= 0  &&  itsInt < 256, AipsError);
    return itsInt;
  case TpFloat:
    AlwaysAssert (itsFloat >= 0  &&  itsFloat < 256, AipsError);
    return uChar(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0  &&  itsDouble < 256, AipsError);
    return uChar(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuChar - invalid data type");
}

Short ValueHolderRep::asShort() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    AlwaysAssert (itsInt >= -32768  &&  itsInt < 32768, AipsError);
    return itsInt;
  case TpFloat:
    AlwaysAssert (itsFloat >= -32768  &&  itsFloat < 32768, AipsError);
    return Short(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= -32768  &&  itsDouble < 32768, AipsError);
    return Short(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asShort - invalid data type");
}

uShort ValueHolderRep::asuShort() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    AlwaysAssert (itsShort >= 0, AipsError);
    return itsShort;
  case TpInt:
    AlwaysAssert (itsInt >= 0  &&  itsInt < 65536, AipsError);
    return itsInt;
  case TpFloat:
    AlwaysAssert (itsFloat >= 0  &&  itsFloat < 65536, AipsError);
    return uShort(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0  &&  itsDouble < 65536, AipsError);
    return uShort(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuShort - invalid data type");
}

Int ValueHolderRep::asInt() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return Int(itsFloat);
  case TpDouble:
    return Int(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asInt - invalid data type");
}

uInt ValueHolderRep::asuInt() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    AlwaysAssert (itsShort >= 0, AipsError);
    return itsShort;
  case TpInt:
    AlwaysAssert (itsInt >= 0, AipsError);
    return itsInt;
  case TpFloat:
    AlwaysAssert (itsFloat >= 0, AipsError);
    return uInt(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0, AipsError);
    return uInt(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuInt - invalid data type");
}

Float ValueHolderRep::asFloat() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return itsFloat;
  case TpDouble:
    return itsDouble;
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asFloat - invalid data type");
}

Double ValueHolderRep::asDouble() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return itsFloat;
  case TpDouble:
    return itsDouble;
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asDouble - invalid data type");
}

Complex ValueHolderRep::asComplex() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return itsFloat;
  case TpDouble:
    return itsDouble;
  case TpComplex:
    return *static_cast<Complex*>(itsPtr);
  case TpDComplex:
    return Complex (static_cast<DComplex*>(itsPtr)->real(),
		    static_cast<DComplex*>(itsPtr)->imag());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asComplex - invalid data type");
}

DComplex ValueHolderRep::asDComplex() const
{
  switch (itsType) {
  case TpUChar:
    return itsUChar;
  case TpShort:
    return itsShort;
  case TpInt:
    return itsInt;
  case TpFloat:
    return itsFloat;
  case TpDouble:
    return itsDouble;
  case TpComplex:
    return *static_cast<Complex*>(itsPtr);
  case TpDComplex:
    return *static_cast<DComplex*>(itsPtr);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asDComplex - invalid data type");
}

const String& ValueHolderRep::asString() const
{
  switch (itsType) {
  case TpString:
    return *static_cast<String*>(itsPtr);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asString - invalid data type");
}

const Array<Bool> ValueHolderRep::asArrayBool() const
{
  if (itsType == TpOther) {
    return Array<Bool>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayBool:
    return *static_cast<Array<Bool>*>(itsPtr);
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpBool:
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<Bool> (1, asBool());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayBool - invalid data type");
}

const Array<uChar> ValueHolderRep::asArrayuChar() const
{
  if (itsType == TpOther) {
    return Array<uChar>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    return *static_cast<Array<uChar>*>(itsPtr);
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<uChar> (1, asuChar());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayuChar - invalid data type");
}

const Array<Short> ValueHolderRep::asArrayShort() const
{
  if (itsType == TpOther) {
    return Array<Short>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    return *static_cast<Array<Short>*>(itsPtr);
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<Short> (1, asShort());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayShort - invalid data type");
}

const Array<uShort> ValueHolderRep::asArrayuShort() const
{
  if (itsType == TpOther) {
    return Array<uShort>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<uShort> (1, asuShort());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayuShort - invalid data type");
}

const Array<Int> ValueHolderRep::asArrayInt() const
{
  if (itsType == TpOther) {
    return Array<Int>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    return *static_cast<Array<Int>*>(itsPtr);
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<Int> (1, asInt());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayInt - invalid data type");
}

const Array<uInt> ValueHolderRep::asArrayuInt() const
{
  if (itsType == TpOther) {
    return Array<uInt>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<uInt> (1, asuInt());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayuInt - invalid data type");
}

const Array<Float> ValueHolderRep::asArrayFloat() const
{
  if (itsType == TpOther) {
    return Array<Float>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    return *static_cast<Array<Float>*>(itsPtr);
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<Float> (1, asFloat());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayFloat - invalid data type");
}

const Array<Double> ValueHolderRep::asArrayDouble() const
{
  if (itsType == TpOther) {
    return Array<Double>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    return *static_cast<Array<Double>*>(itsPtr);
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
    return Vector<Double> (1, asDouble());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayDouble - invalid data type");
}

const Array<Complex> ValueHolderRep::asArrayComplex() const
{
  if (itsType == TpOther) {
    return Array<Complex>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayComplex:
    return *static_cast<Array<Complex>*>(itsPtr);
  case TpArrayDComplex:
    {
      const Array<DComplex>& from = *static_cast<Array<DComplex>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
  case TpComplex:
  case TpDComplex:
    return Vector<Complex> (1, asComplex());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayComplex - invalid data type");
}

const Array<DComplex> ValueHolderRep::asArrayDComplex() const
{
  if (itsType == TpOther) {
    return Array<DComplex>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
    {
      const Array<uChar>& from = *static_cast<Array<uChar>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayShort:
    {
      const Array<Short>& from = *static_cast<Array<Short>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<Float>& from = *static_cast<Array<Float>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<Double>& from = *static_cast<Array<Double>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayComplex:
    {
      const Array<Complex>& from = *static_cast<Array<Complex>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDComplex:
    return *static_cast<Array<DComplex>*>(itsPtr);
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpFloat:
  case TpDouble:
  case TpComplex:
  case TpDComplex:
    return Vector<DComplex> (1, asDComplex());
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayDComplex - invalid data type");
}

const Array<String> ValueHolderRep::asArrayString() const
{
  if (itsType == TpOther) {
    return Array<String>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayString:
    return *static_cast<Array<String>*>(itsPtr);
  case TpString:
    return Vector<String>(1, *static_cast<String*>(itsPtr));
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asArrayString - invalid data type");
}

const Record& ValueHolderRep::asRecord() const
{
  switch (itsType) {
  case TpRecord:
    return *static_cast<Record*>(itsPtr);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asRecord - invalid data type");
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

ostream& ValueHolderRep::write (ostream& os) const
{
  switch (itsType) {
  case TpBool:
    os << itsBool;
    break;
  case TpUChar:
  case TpShort:
  case TpInt:
    os << asInt();
    break;
  case TpFloat:
  case TpDouble:
    os << asDouble();
    break;
  case TpComplex:
  case TpDComplex:
    os << asDComplex();
    break;
  case TpString:
    os << asString();
    break;
  case TpArrayBool:
    os << asArrayBool();
    break;
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayInt:
    os << asArrayInt();
    break;
  case TpArrayFloat:
  case TpArrayDouble:
    os << asArrayDouble();
    break;
  case TpArrayComplex:
  case TpArrayDComplex:
    os << asArrayDComplex();
    break;
  case TpArrayString:
    os << asArrayString();
    break;
  case TpRecord:
    os << asRecord();
    break;
  case TpOther:
    os << "Empty untyped array";
    break;
  default:
    throw AipsError ("ValueHolder::write - unknown data type");
    break;
  }
  return os;
}

} //# NAMESPACE CASA - END
