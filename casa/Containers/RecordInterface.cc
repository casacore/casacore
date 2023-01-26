//# RecordInterface.cc: Abstract base class for a hierarchical collection of named fields of various types
//# Copyright (C) 1996,1998,1999,2000,2001
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


#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RecordInterface::RecordInterface ()
: checkFunction_p (0),
  checkArgument_p (0),
  type_p          (Variable)
{}

RecordInterface::RecordInterface (RecordType type,
				  CheckFieldFunction* funcPtr,
				  const void* checkArgument)
: checkFunction_p (funcPtr),
  checkArgument_p (checkArgument),
  type_p          (type)
{}

RecordInterface::RecordInterface (const RecordInterface& other)
: checkFunction_p (other.checkFunction_p),
  checkArgument_p (other.checkArgument_p),
  type_p          (other.type_p)
{}

RecordInterface& RecordInterface::operator= (const RecordInterface& other)
{
    checkFunction_p = other.checkFunction_p;
    checkArgument_p = other.checkArgument_p;
    type_p          = other.type_p;
    return *this;
}
    
RecordInterface::~RecordInterface()
{
}


void RecordInterface::throwIfFixed() const
{
    if (isFixed()) {
	throw (AipsError ("Record cannot be changed (fixed structure)"));
    }
}

void RecordInterface::checkName (const String& fieldName, DataType type) const
{
    if (checkFunction_p != 0) {
	String message;
	if (! checkFunction_p (fieldName, type, checkArgument_p, message)) {
	    throw (AipsError ("Record field " + fieldName +
			      " cannot be added: " + message));
	}
    }
}


RecordDesc RecordInterface::description() const
{
    return getDescription();
}


int32_t RecordInterface::newIdToNumber (const RecordFieldId& id) const
{
    if (id.byName()) {
	return fieldNumber (id.fieldName());
    }
    int32_t nfield = nfields();
    if (id.fieldNumber() > nfield) {
	throw (AipsError ("RecordInterface::define - "
			  "new fieldNumber exceeds #fields"));
    }
    if (id.fieldNumber() == nfield) {
	return -1;
    }
    return id.fieldNumber();
}
int32_t RecordInterface::idToNumber (const RecordFieldId& id) const
{
    if (! id.byName()) {
	return id.fieldNumber();
    }
    int32_t whichField = fieldNumber (id.fieldName());
    if (whichField < 0) {
	throw (AipsError ("RecordInterface: field " + id.fieldName() +
			  " is unknown"));
    }
    return whichField;
}


String RecordInterface::name (const RecordFieldId& id) const
{
    return description().name (idToNumber (id));
}

IPosition RecordInterface::shape (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    switch (type (whichField)) {
    case TpArrayBool:
	return asArrayBool(whichField).shape();
    case TpArrayUChar:
	return asArrayuChar(whichField).shape();
    case TpArrayShort:
	return asArrayShort(whichField).shape();
    case TpArrayInt:
	return asArrayInt(whichField).shape();
    case TpArrayUInt:
	return asArrayuInt(whichField).shape();
    case TpArrayInt64:
	return asArrayInt64(whichField).shape();
    case TpArrayFloat:
	return asArrayFloat(whichField).shape();
    case TpArrayDouble:
	return asArrayDouble(whichField).shape();
    case TpArrayComplex:
	return asArrayComplex(whichField).shape();
    case TpArrayDComplex:
	return asArrayDComplex(whichField).shape();
    case TpArrayString:
	return asArrayString(whichField).shape();
    default:
	break;
    }
    return IPosition (1,1);
}

void RecordInterface::defineField (const RecordFieldId& id, DataType type,
				   const void* value)
{
    defineField (id, type, IPosition(), false, value);
}
void RecordInterface::defineField (const RecordFieldId& id, DataType type,
				   const IPosition& shape, bool fixedShape,
				   const void* value)
{
    int32_t whichField = newIdToNumber (id);
    if (whichField < 0) {
	throwIfFixed();
	String name;
	if (id.byName()) {
	    name = id.fieldName();
	}else{
	    name = description().makeName (id.fieldNumber());
	}
	checkName (name, type);
	addDataField (name, type, shape, fixedShape, value);
    }else{
	defineDataField (whichField, type, value);
    }
}

void RecordInterface::define (const RecordFieldId& id, bool value)
{
    defineField (id, TpBool, &value);
}
void RecordInterface::define (const RecordFieldId& id, unsigned char value)
{
    defineField (id, TpUChar, &value);
}
void RecordInterface::define (const RecordFieldId& id, int16_t value)
{
    defineField (id, TpShort, &value);
}
void RecordInterface::define (const RecordFieldId& id, int32_t value)
{
    defineField (id, TpInt, &value);
}
void RecordInterface::define (const RecordFieldId& id, uint32_t value)
{
    defineField (id, TpUInt, &value);
}
void RecordInterface::define (const RecordFieldId& id, int64_t value)
{
    defineField (id, TpInt64, &value);
}
void RecordInterface::define (const RecordFieldId& id, float value)
{
    defineField (id, TpFloat, &value);
}
void RecordInterface::define (const RecordFieldId& id, double value)
{
    defineField (id, TpDouble, &value);
}
void RecordInterface::define (const RecordFieldId& id, const Complex& value)
{
    defineField (id, TpComplex, &value);
}
void RecordInterface::define (const RecordFieldId& id, const DComplex& value)
{
    defineField (id, TpDComplex, &value);
}
void RecordInterface::define (const RecordFieldId& id, const String& value)
{
    defineField (id, TpString, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<bool>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayBool, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<unsigned char>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayUChar, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<int16_t>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayShort, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<int32_t>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayInt, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<uint32_t>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayUInt, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<int64_t>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayInt64, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<float>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayFloat, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<double>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayDouble, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<Complex>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayComplex, value.shape(), fixedShape, &value);
} 
void RecordInterface::define (const RecordFieldId& id,
			      const Array<DComplex>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayDComplex, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<String>& value,
			      bool fixedShape)
{
    defineField (id, TpArrayString, value.shape(), fixedShape, &value);
}


void RecordInterface::get (const RecordFieldId& id, bool& value) const
{
    value = asBool (id);
}
void RecordInterface::get (const RecordFieldId& id, unsigned char& value) const
{
    value = asuChar (id);
}
void RecordInterface::get (const RecordFieldId& id, int16_t& value) const
{
    value = asShort (id);
}
void RecordInterface::get (const RecordFieldId& id, int32_t& value) const
{
    value = asInt (id);
}
void RecordInterface::get (const RecordFieldId& id, uint32_t& value) const
{
    value = asuInt (id);
}
void RecordInterface::get (const RecordFieldId& id, int64_t& value) const
{
    value = asInt64 (id);
}
void RecordInterface::get (const RecordFieldId& id, float& value) const
{
    value = asFloat (id);
}
void RecordInterface::get (const RecordFieldId& id, double& value) const
{
    value = asDouble (id);
}
void RecordInterface::get (const RecordFieldId& id, Complex& value) const
{
    value = asComplex (id);
}
void RecordInterface::get (const RecordFieldId& id, DComplex& value) const
{
    value = asDComplex (id);
}
void RecordInterface::get (const RecordFieldId& id, String& value) const
{
    value = asString (id);
}
void RecordInterface::get (const RecordFieldId& id, Array<bool>& value) const
{
    Array<bool> array = toArrayBool (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<unsigned char>& value) const
{
    Array<unsigned char> array = toArrayuChar (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<int16_t>& value) const
{
    Array<int16_t> array = toArrayShort (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<int32_t>& value) const
{
    Array<int32_t> array = toArrayInt (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<uint32_t>& value) const
{
    Array<uint32_t> array = toArrayuInt (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<int64_t>& value) const
{
    Array<int64_t> array = toArrayInt64 (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<float>& value) const
{
    Array<float> array = toArrayFloat (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<double>& value) const
{
    Array<double> array = toArrayDouble (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<Complex>& value) const
{
    Array<Complex> array = toArrayComplex (id);
    value.resize (array.shape());
    value = array;
} 
void RecordInterface::get (const RecordFieldId& id, Array<DComplex>& value) const
{
    Array<DComplex> array = toArrayDComplex (id);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<String>& value) const
{
    Array<String> array = toArrayString (id);
    value.resize (array.shape());
    value = array;
}


bool RecordInterface::asBool (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpBool:
        break;
    case TpInt:
        return *(const int32_t*)get_pointer (whichField, TpInt);
    default:
        throw (AipsError ("RecordInterface::asBool - invalid data type"));
    }
    return *(const bool*)get_pointer (whichField, TpBool);
}
unsigned char RecordInterface::asuChar (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	break;
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    default:
	throw (AipsError ("RecordInterface::asuChar - invalid data type"));
    }
    return *(const unsigned char*)get_pointer (whichField, TpUChar);
}
int16_t RecordInterface::asShort (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	break;
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    default:
	throw (AipsError ("RecordInterface::asShort - invalid data type"));
    }
    return *(const int16_t*)get_pointer (whichField, TpShort);
}
int32_t RecordInterface::asInt (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
        break;
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    default:
	throw (AipsError ("RecordInterface::asInt - invalid data type"));
    }
    return *(const int32_t*)get_pointer (whichField, TpInt);
}
uint32_t RecordInterface::asuInt (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	break;
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    default:
	throw (AipsError ("RecordInterface::asuInt - invalid data type"));
    }
    return *(const uint32_t*)get_pointer (whichField, TpUInt);
}
int64_t RecordInterface::asInt64 (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	break;
    default:
	throw (AipsError ("RecordInterface::asInt64 - invalid data type"));
    }
    return *(const int64_t*)get_pointer (whichField, TpInt64);
}
float RecordInterface::asFloat (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    case TpFloat:
	break;
    case TpDouble:
	return *(const double*)get_pointer (whichField, TpDouble);
    default:
	throw (AipsError ("RecordInterface::asFloat - invalid data type"));
    }
    return *(const float*)get_pointer (whichField, TpFloat);
}
double RecordInterface::asDouble (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    case TpFloat:
	return *(const float*)get_pointer (whichField, TpFloat);
    case TpDouble:
	break;
    default:
	throw (AipsError ("RecordInterface::asDouble - invalid data type"));
    }
    return *(const double*)get_pointer (whichField, TpDouble);
}
Complex RecordInterface::asComplex (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    case TpFloat:
	return *(const float*)get_pointer (whichField, TpFloat);
    case TpDouble:
	return *(const double*)get_pointer (whichField, TpDouble);
    case TpComplex:
	break;
    case TpDComplex:
      {
	DComplex dc = *(const DComplex*)get_pointer (whichField, TpDComplex);
	return Complex(dc.real(), dc.imag());
      }
    default:
	throw (AipsError ("RecordInterface::asComplex - invalid data type"));
    }
    return *(const Complex*)get_pointer (whichField, TpComplex);
}
DComplex RecordInterface::asDComplex (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const unsigned char*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const int16_t*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const int32_t*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uint32_t*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const int64_t*)get_pointer (whichField, TpInt64);
    case TpFloat:
	return *(const float*)get_pointer (whichField, TpFloat);
    case TpDouble:
	return *(const double*)get_pointer (whichField, TpDouble);
    case TpComplex:
	{
	    const Complex* val = (const Complex*)get_pointer (whichField,
							      TpComplex);
	    return DComplex (val->real(), val->imag());
	}
    case TpDComplex:
	break;
    default:
	throw (AipsError ("RecordInterface::asDComplex - invalid data type"));
    }
    return *(const DComplex*)get_pointer (whichField, TpDComplex);
}
const String& RecordInterface::asString (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const String*)get_pointer (whichField, TpString);
}
const Array<bool>& RecordInterface::asArrayBool (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<bool>*)get_pointer (whichField, TpArrayBool);
}
const Array<unsigned char>& RecordInterface::asArrayuChar (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<unsigned char>*)get_pointer (whichField, TpArrayUChar);
}
const Array<int16_t>& RecordInterface::asArrayShort (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<int16_t>*)get_pointer (whichField, TpArrayShort);
}
const Array<int32_t>& RecordInterface::asArrayInt (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<int32_t>*)get_pointer (whichField, TpArrayInt);
}
const Array<uint32_t>& RecordInterface::asArrayuInt (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<uint32_t>*)get_pointer (whichField, TpArrayUInt);
}
const Array<int64_t>& RecordInterface::asArrayInt64 (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<int64_t>*)get_pointer (whichField, TpArrayInt64);
}
const Array<float>& RecordInterface::asArrayFloat (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<float>*)get_pointer (whichField, TpArrayFloat);
}
const Array<double>& RecordInterface::asArrayDouble (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<double>*)get_pointer (whichField, TpArrayDouble);
}
const Array<Complex>& RecordInterface::asArrayComplex (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<Complex>*)get_pointer (whichField, TpArrayComplex);
} 
const Array<DComplex>&  RecordInterface::asArrayDComplex (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<DComplex>*)get_pointer (whichField, TpArrayDComplex);
}
const Array<String>& RecordInterface::asArrayString (const RecordFieldId& id) const
{
    int32_t whichField = idToNumber (id);
    return *(const Array<String>*)get_pointer (whichField, TpArrayString);
}

} //# NAMESPACE CASACORE - END

