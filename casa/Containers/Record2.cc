//# Record2.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 2006
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

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ValueHolder Record::asValueHolder (const RecordFieldId& id) const
{
  switch (dataType(id)) {
  case TpBool:
    return ValueHolder(asBool(id));
  case TpUChar:
    return ValueHolder(asuChar(id));
  case TpShort:
    return ValueHolder(asShort(id));
  case TpInt:
    return ValueHolder(asInt(id));
  case TpUInt:
    return ValueHolder(asuInt(id));
  case TpInt64:
    return ValueHolder(asInt64(id));
  case TpFloat:
    return ValueHolder(asFloat(id));
  case TpDouble:
    return ValueHolder(asDouble(id));
  case TpComplex:
    return ValueHolder(asComplex(id));
  case TpDComplex:
    return ValueHolder(asDComplex(id));
  case TpString:
    return ValueHolder(asString(id));
  case TpArrayBool:
    return ValueHolder(asArrayBool(id));
  case TpArrayUChar:
    return ValueHolder(asArrayuChar(id));
  case TpArrayShort:
    return ValueHolder(asArrayShort(id));
  case TpArrayInt:
    return ValueHolder(asArrayInt(id));
  case TpArrayUInt:
    return ValueHolder(asArrayuInt(id));
  case TpArrayInt64:
    return ValueHolder(asArrayInt64(id));
  case TpArrayFloat:
    return ValueHolder(asArrayFloat(id));
  case TpArrayDouble:
    return ValueHolder(asArrayDouble(id));
  case TpArrayComplex:
    return ValueHolder(asArrayComplex(id));
  case TpArrayDComplex:
    return ValueHolder(asArrayDComplex(id));
  case TpArrayString:
    return ValueHolder(asArrayString(id));
  case TpRecord:
    return ValueHolder(subRecord(id));
  default:
    break;
  }
  throw AipsError ("Record::asValueHolder - unknown data type");
}

void Record::defineFromValueHolder (const RecordFieldId& id,
				    const ValueHolder& value)
{
  switch (value.dataType()) {
  case TpBool:
    define (id, value.asBool());
    break;
  case TpUChar:
    define (id, value.asuChar());
    break;
  case TpShort:
    define (id, value.asShort());
    break;
  case TpUShort:
  case TpInt:
    define (id, value.asInt());
    break;
  case TpUInt:
    define (id, value.asuInt());
    break;
  case TpInt64:
    define (id, value.asInt64());
    break;
  case TpFloat:
    define (id, value.asFloat());
    break;
  case TpDouble:
    define (id, value.asDouble());
    break;
  case TpComplex:
    define (id, value.asComplex());
    break;
  case TpDComplex:
    define (id, value.asDComplex());
    break;
  case TpString:
    define (id, value.asString());
    break;
  case TpArrayBool:
    define (id, value.asArrayBool());
    break;
  case TpArrayUChar:
    define (id, value.asArrayuChar());
    break;
  case TpArrayShort:
    define (id, value.asArrayShort());
    break;
  case TpArrayUShort:
  case TpArrayInt:
    define (id, value.asArrayInt());
    break;
  case TpArrayUInt:
    define (id, value.asArrayuInt());
    break;
  case TpArrayInt64:
    define (id, value.asArrayInt64());
    break;
  case TpArrayFloat:
    define (id, value.asArrayFloat());
    break;
  case TpArrayDouble:
    define (id, value.asArrayDouble());
    break;
  case TpArrayComplex:
    define (id, value.asArrayComplex());
    break;
  case TpArrayDComplex:
    define (id, value.asArrayDComplex());
    break;
  case TpArrayString:
    define (id, value.asArrayString());
    break;
  case TpRecord:
    defineRecord (id, value.asRecord());
    break;
  case TpOther:
    // An untyped array is handled as an Int array.
    define (id, value.asArrayInt());
    break;
  default:
    throw AipsError ("Record::defineFromValueHolder - unknown data type");
    break;
  }
}

} //# NAMESPACE CASACORE - END

