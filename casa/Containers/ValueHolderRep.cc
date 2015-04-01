//# ValueHolderRep.cc: A holder object for the standard Casacore data
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


#include <casacore/casa/Containers/ValueHolderRep.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


ValueHolderRep::ValueHolderRep (Bool value)
  : itsNdim (0),
    itsType (TpBool),
    itsBool (value)
{}

ValueHolderRep::ValueHolderRep (uChar value)
  : itsNdim (0),
    itsType (TpUChar),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (Short value)
  : itsNdim (0),
    itsType (TpShort),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (uShort value)
  : itsNdim (0),
    itsType (TpUShort),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (Int value)
  : itsNdim (0),
    itsType (TpInt),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (uInt value)
  : itsNdim (0),
    itsType (TpUInt),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (Int64 value)
  : itsNdim (0),
    itsType (TpInt64),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (Float value)
  : itsNdim (0),
    itsType  (TpFloat),
    itsFloat (value)
{}

ValueHolderRep::ValueHolderRep (Double value)
  : itsNdim (0),
    itsType   (TpDouble),
    itsDouble (value)
{}

ValueHolderRep::ValueHolderRep (const Complex& value)
  : itsNdim (0),
    itsType (TpComplex),
    itsPtr  (new Complex(value))
{}

ValueHolderRep::ValueHolderRep (const DComplex& value)
  : itsNdim (0),
    itsType (TpDComplex),
    itsPtr  (new DComplex(value))
{}

ValueHolderRep::ValueHolderRep (const String& value)
  : itsNdim (0),
    itsType (TpString),
    itsPtr  (new String(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Bool>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayBool),
    itsPtr  (new Array<Bool>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uChar>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayUChar),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray(*(static_cast<Array<Int>*>(itsPtr)), value);
}

ValueHolderRep::ValueHolderRep (const Array<Short>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayShort),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray(*(static_cast<Array<Int>*>(itsPtr)), value);
}

ValueHolderRep::ValueHolderRep (const Array<uShort>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value.shape()))
{
  convertArray (*static_cast<Array<Int>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<Int>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<Int>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uInt>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayUInt),
    itsPtr  (new Array<uInt>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Int64>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayInt64),
    itsPtr  (new Array<Int64>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Float>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayFloat),
    itsPtr  (new Array<Float>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Double>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayDouble),
    itsPtr  (new Array<Double>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<Complex>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayComplex),
    itsPtr  (new Array<Complex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<DComplex>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayDComplex),
    itsPtr  (new Array<DComplex>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<String>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayString),
    itsPtr  (new Array<String>(value))
{}

ValueHolderRep::ValueHolderRep (const Record& value)
  : itsNdim (0),
    itsType (TpRecord),
    itsPtr  (new Record(value))
{}

ValueHolderRep::ValueHolderRep (uInt ndim, Bool)
  : itsNdim (ndim),
    itsType (TpOther),
    itsPtr  (0)
{}

ValueHolderRep::~ValueHolderRep()
{
  switch (itsType) {
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
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    delete static_cast<Array<Int>*>(itsPtr);
    break;
  case TpArrayUInt:
    delete static_cast<Array<uInt>*>(itsPtr);
    break;
  case TpArrayInt64:
    delete static_cast<Array<Int64>*>(itsPtr);
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    AlwaysAssert (itsInt64 >= 0  &&  itsInt64 < 256, AipsError);
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    AlwaysAssert (itsInt64 >= -32768  &&  itsInt64 < 32768, AipsError);
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    AlwaysAssert (itsInt64 >= 0  &&  itsInt64 < 65536, AipsError);
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return Int(itsInt64);
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    AlwaysAssert (itsInt64 >= 0, AipsError);
    return itsInt64;
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

Int64 ValueHolderRep::asInt64() const
{
  switch (itsType) {
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
  case TpFloat:
    return static_cast<Int64> (itsFloat);
  case TpDouble:
    return static_cast<Int64> (itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asInt64 - invalid data type");
}

Float ValueHolderRep::asFloat() const
{
  switch (itsType) {
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
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
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64;
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
  // Empty array from numpy (which is untyped).
  if (itsType == TpOther) {
    return Array<Bool>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayBool:
    return *static_cast<Array<Bool>*>(itsPtr);
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<Bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<Bool> (1, asBool());
  }
}

const Array<uChar> ValueHolderRep::asArrayuChar() const
{
  if (itsType == TpOther) {
    return Array<uChar>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<uChar> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<uChar> (1, asuChar());
  }
}

const Array<Short> ValueHolderRep::asArrayShort() const
{
  if (itsType == TpOther) {
    return Array<Short>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<Short> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<Short> (1, asShort());
  }
}

const Array<uShort> ValueHolderRep::asArrayuShort() const
{
  if (itsType == TpOther) {
    return Array<uShort>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<uShort> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<uShort> (1, asuShort());
  }
}

const Array<Int> ValueHolderRep::asArrayInt() const
{
  if (itsType == TpOther) {
    return Array<Int>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    return *static_cast<Array<Int>*>(itsPtr);
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<Int> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<Int> (1, asInt());
  }
}

const Array<uInt> ValueHolderRep::asArrayuInt() const
{
  if (itsType == TpOther) {
    return Array<uInt>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    return *static_cast<Array<uInt>*>(itsPtr);
  case TpArrayInt64:
    {
      const Array<Int64> from = asArrayInt64();
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<uInt> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<uInt> (1, asuInt());
  }
}

const Array<Int64> ValueHolderRep::asArrayInt64() const
{
  if (itsType == TpOther) {
    return Array<Int64>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Int64> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uInt> from = asArrayuInt();
      Array<Int64> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    return *static_cast<Array<Int64>*>(itsPtr);
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<Double> from = asArrayDouble();
      Array<Int64> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<Int64> (1, asInt64());
  }
}

const Array<Float> ValueHolderRep::asArrayFloat() const
{
  if (itsType == TpOther) {
    return Array<Float>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uInt>& from = *static_cast<Array<uInt>*>(itsPtr);
      Array<Float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<Int64>& from = *static_cast<Array<Int64>*>(itsPtr);
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
  default:
    return Vector<Float> (1, asFloat());
  }
}

const Array<Double> ValueHolderRep::asArrayDouble() const
{
  if (itsType == TpOther) {
    return Array<Double>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uInt>& from = *static_cast<Array<uInt>*>(itsPtr);
      Array<Double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<Int64>& from = *static_cast<Array<Int64>*>(itsPtr);
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
  default:
    return Vector<Double> (1, asDouble());
  }
}

const Array<Complex> ValueHolderRep::asArrayComplex() const
{
  if (itsType == TpOther) {
    return Array<Complex>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uInt>& from = *static_cast<Array<uInt>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<Int64>& from = *static_cast<Array<Int64>*>(itsPtr);
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
  default:
    return Vector<Complex> (1, asComplex());
  }
}

const Array<DComplex> ValueHolderRep::asArrayDComplex() const
{
  if (itsType == TpOther) {
    return Array<DComplex>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uInt>& from = *static_cast<Array<uInt>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<Int64>& from = *static_cast<Array<Int64>*>(itsPtr);
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
  default:
    return Vector<DComplex> (1, asDComplex());
  }
}

const Array<String> ValueHolderRep::asArrayString() const
{
  if (itsType == TpOther) {
    return Array<String>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayString:
    return *static_cast<Array<String>*>(itsPtr);
  default:
    return Vector<String> (1, asString());
  }
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
    rec.define (id, asuChar());
    break;
  case TpShort:
    rec.define (id, asShort());
    break;
  case TpUShort:
  case TpInt:
    rec.define (id, asInt());
    break;
  case TpUInt:
    rec.define (id, asuInt());
    break;
  case TpInt64:
    rec.define (id, itsInt64);
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
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<uChar> to(from.shape());
      convertArray (to, from);
      rec.define (id, to);
      break;
    }
  case TpArrayShort:
    {
      const Array<Int>& from = *static_cast<Array<Int>*>(itsPtr);
      Array<Short> to(from.shape());
      convertArray (to, from);
      rec.define (id, to);
      break;
    }
  case TpArrayUShort:
  case TpArrayInt:
    rec.define (id, *static_cast<Array<Int>*>(itsPtr));
    break;
  case TpArrayUInt:
    rec.define (id, *static_cast<Array<uInt>*>(itsPtr));
    break;
  case TpArrayInt64:
    rec.define (id, *static_cast<Array<Int64>*>(itsPtr));
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
  case TpUInt:
    return new ValueHolderRep (rec.asuInt(id));
  case TpInt64:
    return new ValueHolderRep (rec.asInt64(id));
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
  case TpArrayUInt:
    return new ValueHolderRep (rec.asArrayuInt(id));
  case TpArrayInt64:
    return new ValueHolderRep (rec.asArrayInt64(id));
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
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    os << asInt64();
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
  case TpArrayUShort:
  case TpArrayInt:
  case TpArrayUInt:
  case TpArrayInt64:
    os << asArrayInt64();
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

bool ValueHolderRep::operator< (const ValueHolderRep& right) const
{
  AlwaysAssert (itsType == right.itsType, AipsError);
  switch (itsType) {
  case TpBool:
    return itsBool < right.itsBool;
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return itsInt64 < right.itsInt64;
  case TpFloat:
    return itsFloat < right.itsFloat;
  case TpDouble:
    return itsDouble < right.itsDouble;
  case TpString:
    return *static_cast<String*>(itsPtr) < *static_cast<String*>(right.itsPtr);
  default:
    throw AipsError ("ValueHolder::operator< - unsupported data type");
  }
}

} //# NAMESPACE CASACORE - END
