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


#include <casacore/casa/Containers/ValueHolderRep.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


ValueHolderRep::ValueHolderRep (bool value)
  : itsNdim (0),
    itsType (TpBool),
    itsBool (value)
{}

ValueHolderRep::ValueHolderRep (unsigned char value)
  : itsNdim (0),
    itsType (TpUChar),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (int16_t value)
  : itsNdim (0),
    itsType (TpShort),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (uint16_t value)
  : itsNdim (0),
    itsType (TpUShort),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (int32_t value)
  : itsNdim (0),
    itsType (TpInt),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (uint32_t value)
  : itsNdim (0),
    itsType (TpUInt),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (int64_t value)
  : itsNdim (0),
    itsType (TpInt64),
    itsInt64(value)
{}

ValueHolderRep::ValueHolderRep (float value)
  : itsNdim (0),
    itsType  (TpFloat),
    itsFloat (value)
{}

ValueHolderRep::ValueHolderRep (double value)
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

ValueHolderRep::ValueHolderRep (const Array<bool>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayBool),
    itsPtr  (new Array<bool>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<unsigned char>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayUChar),
    itsPtr  (new Array<int32_t>(value.shape()))
{
  convertArray(*(static_cast<Array<int32_t>*>(itsPtr)), value);
}

ValueHolderRep::ValueHolderRep (const Array<int16_t>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayShort),
    itsPtr  (new Array<int32_t>(value.shape()))
{
  convertArray(*(static_cast<Array<int32_t>*>(itsPtr)), value);
}

ValueHolderRep::ValueHolderRep (const Array<uint16_t>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayUInt),
    itsPtr  (new Array<uint32_t>(value.shape()))
{
  convertArray (*static_cast<Array<uint32_t>*>(itsPtr), value);
}

ValueHolderRep::ValueHolderRep (const Array<int32_t>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayInt),
    itsPtr  (new Array<int32_t>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<uint32_t>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayUInt),
    itsPtr  (new Array<uint32_t>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<int64_t>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayInt64),
    itsPtr  (new Array<int64_t>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<float>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayFloat),
    itsPtr  (new Array<float>(value))
{}

ValueHolderRep::ValueHolderRep (const Array<double>& value)
  : itsNdim (value.ndim()),
    itsType (TpArrayDouble),
    itsPtr  (new Array<double>(value))
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

ValueHolderRep::ValueHolderRep (uint32_t ndim, bool)
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
    delete static_cast<Array<bool>*>(itsPtr);
    break;
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    delete static_cast<Array<int32_t>*>(itsPtr);
    break;
  case TpArrayUInt:
    delete static_cast<Array<uint32_t>*>(itsPtr);
    break;
  case TpArrayInt64:
    delete static_cast<Array<int64_t>*>(itsPtr);
    break;
  case TpArrayFloat:
    delete static_cast<Array<float>*>(itsPtr);
    break;
  case TpArrayDouble:
    delete static_cast<Array<double>*>(itsPtr);
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


bool ValueHolderRep::asBool() const
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
  throw AipsError ("ValueHolderRep::asBool - invalid data type " +
                   String::toString(itsType));
}

unsigned char ValueHolderRep::asuChar() const
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
    return static_cast<unsigned char>(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0  &&  itsDouble < 256, AipsError);
    return static_cast<unsigned char>(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuChar - invalid data type " +
                   String::toString(itsType));
}

int16_t ValueHolderRep::asShort() const
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
    return int16_t(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= -32768  &&  itsDouble < 32768, AipsError);
    return int16_t(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asShort - invalid data type " +
                   String::toString(itsType));
}

uint16_t ValueHolderRep::asuShort() const
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
    return uint16_t(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0  &&  itsDouble < 65536, AipsError);
    return uint16_t(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuShort - invalid data type " +
                   String::toString(itsType));
}

int32_t ValueHolderRep::asInt() const
{
  switch (itsType) {
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    return int32_t(itsInt64);
  case TpFloat:
    return int32_t(itsFloat);
  case TpDouble:
    return int32_t(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asInt - invalid data type " +
                   String::toString(itsType));
}

uint32_t ValueHolderRep::asuInt() const
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
    return uint32_t(itsFloat);
  case TpDouble:
    AlwaysAssert (itsDouble >= 0, AipsError);
    return uint32_t(itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asuInt - invalid data type " +
                   String::toString(itsType));
}

int64_t ValueHolderRep::asInt64() const
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
    return static_cast<int64_t> (itsFloat);
  case TpDouble:
    return static_cast<int64_t> (itsDouble);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asInt64 - invalid data type " +
                   String::toString(itsType));
}

float ValueHolderRep::asFloat() const
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
  throw AipsError ("ValueHolderRep::asFloat - invalid data type " +
                   String::toString(itsType));
}

double ValueHolderRep::asDouble() const
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
  throw AipsError ("ValueHolderRep::asDouble - invalid data type " +
                   String::toString(itsType));
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
  throw AipsError ("ValueHolderRep::asComplex - invalid data type " +
                   String::toString(itsType));
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
  throw AipsError ("ValueHolderRep::asDComplex - invalid data type " +
                   String::toString(itsType));
}

const String& ValueHolderRep::asString() const
{
  switch (itsType) {
  case TpString:
    return *static_cast<String*>(itsPtr);
  default:
    ;
  }
  throw AipsError ("ValueHolderRep::asString - invalid data type " +
                   String::toString(itsType));
}

const Array<bool> ValueHolderRep::asArrayBool() const
{
  // Empty array from numpy (which is untyped).
  if (itsType == TpOther) {
    return Array<bool>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayBool:
    return *static_cast<Array<bool>*>(itsPtr);
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<bool> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<bool> (1, asBool());
  }
}

const Array<unsigned char> ValueHolderRep::asArrayuChar() const
{
  if (itsType == TpOther) {
    return Array<unsigned char>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<unsigned char> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<unsigned char> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<unsigned char> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<unsigned char> (1, asuChar());
  }
}

const Array<int16_t> ValueHolderRep::asArrayShort() const
{
  if (itsType == TpOther) {
    return Array<int16_t>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<int16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<int16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<int16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<int16_t> (1, asShort());
  }
}

const Array<uint16_t> ValueHolderRep::asArrayuShort() const
{
  if (itsType == TpOther) {
    return Array<uint16_t>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<uint16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<uint16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<uint16_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<uint16_t> (1, asuShort());
  }
}

const Array<int32_t> ValueHolderRep::asArrayInt() const
{
  if (itsType == TpOther) {
    return Array<int32_t>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    return *static_cast<Array<int32_t>*>(itsPtr);
  case TpArrayUInt:
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<int32_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<int32_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<int32_t> (1, asInt());
  }
}

const Array<uint32_t> ValueHolderRep::asArrayuInt() const
{
  if (itsType == TpOther) {
    return Array<uint32_t>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<uint32_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    return *static_cast<Array<uint32_t>*>(itsPtr);
  case TpArrayInt64:
    {
      const Array<int64_t> from = asArrayInt64();
      Array<uint32_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<uint32_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<uint32_t> (1, asuInt());
  }
}

const Array<int64_t> ValueHolderRep::asArrayInt64() const
{
  if (itsType == TpOther) {
    return Array<int64_t>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<int64_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uint32_t> from = asArrayuInt();
      Array<int64_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    return *static_cast<Array<int64_t>*>(itsPtr);
  case TpArrayFloat:
  case TpArrayDouble:
    {
      const Array<double> from = asArrayDouble();
      Array<int64_t> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<int64_t> (1, asInt64());
  }
}

const Array<float> ValueHolderRep::asArrayFloat() const
{
  if (itsType == TpOther) {
    return Array<float>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uint32_t>& from = *static_cast<Array<uint32_t>*>(itsPtr);
      Array<float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<int64_t>& from = *static_cast<Array<int64_t>*>(itsPtr);
      Array<float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    return *static_cast<Array<float>*>(itsPtr);
  case TpArrayDouble:
    {
      const Array<double>& from = *static_cast<Array<double>*>(itsPtr);
      Array<float> to(from.shape());
      convertArray (to, from);
      return to;
    }
  default:
    return Vector<float> (1, asFloat());
  }
}

const Array<double> ValueHolderRep::asArrayDouble() const
{
  if (itsType == TpOther) {
    return Array<double>(IPosition(itsNdim, 0));
  }
  switch (itsType) {
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uint32_t>& from = *static_cast<Array<uint32_t>*>(itsPtr);
      Array<double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<int64_t>& from = *static_cast<Array<int64_t>*>(itsPtr);
      Array<double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<float>& from = *static_cast<Array<float>*>(itsPtr);
      Array<double> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    return *static_cast<Array<double>*>(itsPtr);
  default:
    return Vector<double> (1, asDouble());
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
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uint32_t>& from = *static_cast<Array<uint32_t>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<int64_t>& from = *static_cast<Array<int64_t>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<float>& from = *static_cast<Array<float>*>(itsPtr);
      Array<Complex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<double>& from = *static_cast<Array<double>*>(itsPtr);
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
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayUInt:
    {
      const Array<uint32_t>& from = *static_cast<Array<uint32_t>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayInt64:
    {
      const Array<int64_t>& from = *static_cast<Array<int64_t>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayFloat:
    {
      const Array<float>& from = *static_cast<Array<float>*>(itsPtr);
      Array<DComplex> to(from.shape());
      convertArray (to, from);
      return to;
    }
  case TpArrayDouble:
    {
      const Array<double>& from = *static_cast<Array<double>*>(itsPtr);
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
  throw AipsError ("ValueHolderRep::asRecord - invalid data type " +
                   String::toString(itsType));
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
    rec.define (id, *static_cast<Array<bool>*>(itsPtr));
    break;
  case TpArrayUChar:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<unsigned char> to(from.shape());
      convertArray (to, from);
      rec.define (id, to);
      break;
    }
  case TpArrayShort:
    {
      const Array<int32_t>& from = *static_cast<Array<int32_t>*>(itsPtr);
      Array<int16_t> to(from.shape());
      convertArray (to, from);
      rec.define (id, to);
      break;
    }
  case TpArrayUShort:
  case TpArrayInt:
    rec.define (id, *static_cast<Array<int32_t>*>(itsPtr));
    break;
  case TpArrayUInt:
    rec.define (id, *static_cast<Array<uint32_t>*>(itsPtr));
    break;
  case TpArrayInt64:
    rec.define (id, *static_cast<Array<int64_t>*>(itsPtr));
    break;
  case TpArrayFloat:
    rec.define (id, *static_cast<Array<float>*>(itsPtr));
    break;
  case TpArrayDouble:
    rec.define (id, *static_cast<Array<double>*>(itsPtr));
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
  throw AipsError ("ValueHolder::fromRecord - unknown data type " +
                   String::toString(rec.dataType(id)));
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
    throw AipsError ("ValueHolder::write - unknown data type " +
                     String::toString(itsType));
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
    throw AipsError ("ValueHolder::operator< - unsupported data type " +
                     String::toString(itsType));
  }
}

} //# NAMESPACE CASACORE - END
