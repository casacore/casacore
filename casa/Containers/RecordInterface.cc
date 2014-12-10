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
//#
//#
//# $Id$


#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Register.h>
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
: NoticeSource    (),
  checkFunction_p (other.checkFunction_p),
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
    notify (RecordNotice (RecordNotice::DETACH, 0));
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


Int RecordInterface::newIdToNumber (const RecordFieldId& id) const
{
    if (id.byName()) {
	return fieldNumber (id.fieldName());
    }
    Int nfield = nfields();
    if (id.fieldNumber() > nfield) {
	throw (AipsError ("RecordInterface::define - "
			  "new fieldNumber exceeds #fields"));
    }
    if (id.fieldNumber() == nfield) {
	return -1;
    }
    return id.fieldNumber();
}
Int RecordInterface::idToNumber (const RecordFieldId& id) const
{
    if (! id.byName()) {
	return id.fieldNumber();
    }
    Int whichField = fieldNumber (id.fieldName());
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
    Int whichField = idToNumber (id);
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
    defineField (id, type, IPosition(), False, value);
}
void RecordInterface::defineField (const RecordFieldId& id, DataType type,
				   const IPosition& shape, Bool fixedShape,
				   const void* value)
{
    Int whichField = newIdToNumber (id);
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

void RecordInterface::define (const RecordFieldId& id, Bool value)
{
    defineField (id, TpBool, &value);
}
void RecordInterface::define (const RecordFieldId& id, uChar value)
{
    defineField (id, TpUChar, &value);
}
void RecordInterface::define (const RecordFieldId& id, Short value)
{
    defineField (id, TpShort, &value);
}
void RecordInterface::define (const RecordFieldId& id, Int value)
{
    defineField (id, TpInt, &value);
}
void RecordInterface::define (const RecordFieldId& id, uInt value)
{
    defineField (id, TpUInt, &value);
}
void RecordInterface::define (const RecordFieldId& id, Int64 value)
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
			      const Array<Bool>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayBool, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<uChar>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayUChar, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<Short>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayShort, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<Int>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayInt, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<uInt>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayUInt, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<Int64>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayInt64, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<float>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayFloat, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<double>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayDouble, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<Complex>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayComplex, value.shape(), fixedShape, &value);
} 
void RecordInterface::define (const RecordFieldId& id,
			      const Array<DComplex>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayDComplex, value.shape(), fixedShape, &value);
}
void RecordInterface::define (const RecordFieldId& id,
			      const Array<String>& value,
			      Bool fixedShape)
{
    defineField (id, TpArrayString, value.shape(), fixedShape, &value);
}


void RecordInterface::get (const RecordFieldId& id, Bool& value) const
{
    value = asBool (id);
}
void RecordInterface::get (const RecordFieldId& id, uChar& value) const
{
    value = asuChar (id);
}
void RecordInterface::get (const RecordFieldId& id, Short& value) const
{
    value = asShort (id);
}
void RecordInterface::get (const RecordFieldId& id, Int& value) const
{
    value = asInt (id);
}
void RecordInterface::get (const RecordFieldId& id, uInt& value) const
{
    value = asuInt (id);
}
void RecordInterface::get (const RecordFieldId& id, Int64& value) const
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
void RecordInterface::get (const RecordFieldId& id, Array<Bool>& value) const
{
    Int whichField = idToNumber (id);
    const Array<Bool>& array = *(const Array<Bool>*)
	                            get_pointer (whichField, TpArrayBool);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<uChar>& value) const
{
    Int whichField = idToNumber (id);
    const Array<uChar>& array = *(const Array<uChar>*)
	                            get_pointer (whichField, TpArrayUChar);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<Short>& value) const
{
    Int whichField = idToNumber (id);
    const Array<Short>& array = *(const Array<Short>*)
	                            get_pointer (whichField, TpArrayShort);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<Int>& value) const
{
    Int whichField = idToNumber (id);
    const Array<Int>& array = *(const Array<Int>*)
	                            get_pointer (whichField, TpArrayInt);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<uInt>& value) const
{
    Int whichField = idToNumber (id);
    const Array<uInt>& array = *(const Array<uInt>*)
	                            get_pointer (whichField, TpArrayUInt);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<Int64>& value) const
{
    Int whichField = idToNumber (id);
    const Array<Int64>& array = *(const Array<Int64>*)
	                            get_pointer (whichField, TpArrayInt64);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<float>& value) const
{
    Int whichField = idToNumber (id);
    const Array<float>& array = *(const Array<float>*)
	                            get_pointer (whichField, TpArrayFloat);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<double>& value) const
{
    Int whichField = idToNumber (id);
    const Array<double>& array = *(const Array<double>*)
	                            get_pointer (whichField, TpArrayDouble);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<Complex>& value) const
{
    Int whichField = idToNumber (id);
    const Array<Complex>& array = *(const Array<Complex>*)
	                            get_pointer (whichField, TpArrayComplex);
    value.resize (array.shape());
    value = array;
} 
void RecordInterface::get (const RecordFieldId& id, Array<DComplex>& value) const
{
    Int whichField = idToNumber (id);
    const Array<DComplex>& array = *(const Array<DComplex>*)
	                            get_pointer (whichField, TpArrayDComplex);
    value.resize (array.shape());
    value = array;
}
void RecordInterface::get (const RecordFieldId& id, Array<String>& value) const
{
    Int whichField = idToNumber (id);
    const Array<String>& array = *(const Array<String>*)
	                            get_pointer (whichField, TpArrayString);
    value.resize (array.shape());
    value = array;
}


Bool RecordInterface::asBool (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpBool:
        break;
    case TpInt:
        return *(const Int*)get_pointer (whichField, TpInt);
    default:
        throw (AipsError ("RecordInterface::asBool - invalid data type"));
    }
    return *(const Bool*)get_pointer (whichField, TpBool);
}
uChar RecordInterface::asuChar (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	break;
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    default:
	throw (AipsError ("RecordInterface::asuChar - invalid data type"));
    }
    return *(const uChar*)get_pointer (whichField, TpUChar);
}
Short RecordInterface::asShort (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	break;
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    default:
	throw (AipsError ("RecordInterface::asShort - invalid data type"));
    }
    return *(const Short*)get_pointer (whichField, TpShort);
}
Int RecordInterface::asInt (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
        break;
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    default:
	throw (AipsError ("RecordInterface::asInt - invalid data type"));
    }
    return *(const Int*)get_pointer (whichField, TpInt);
}
uInt RecordInterface::asuInt (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	break;
    default:
	throw (AipsError ("RecordInterface::asuInt - invalid data type"));
    }
    return *(const uInt*)get_pointer (whichField, TpUInt);
}
Int64 RecordInterface::asInt64 (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    case TpInt64:
	break;
    default:
	throw (AipsError ("RecordInterface::asInt64 - invalid data type"));
    }
    return *(const Int64*)get_pointer (whichField, TpInt64);
}
float RecordInterface::asFloat (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const Int64*)get_pointer (whichField, TpInt64);
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
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const Int64*)get_pointer (whichField, TpInt64);
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
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const Int64*)get_pointer (whichField, TpInt64);
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
    Int whichField = idToNumber (id);
    DataType dataType = type (whichField);
    switch (dataType) {
    case TpUChar:
	return *(const uChar*)get_pointer (whichField, TpUChar);
    case TpShort:
	return *(const Short*)get_pointer (whichField, TpShort);
    case TpInt:
	return *(const Int*)get_pointer (whichField, TpInt);
    case TpUInt:
	return *(const uInt*)get_pointer (whichField, TpUInt);
    case TpInt64:
	return *(const Int64*)get_pointer (whichField, TpInt64);
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
    Int whichField = idToNumber (id);
    return *(const String*)get_pointer (whichField, TpString);
}
const Array<Bool>& RecordInterface::asArrayBool (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<Bool>*)get_pointer (whichField, TpArrayBool);
}
const Array<uChar>& RecordInterface::asArrayuChar (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<uChar>*)get_pointer (whichField, TpArrayUChar);
}
const Array<Short>& RecordInterface::asArrayShort (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<Short>*)get_pointer (whichField, TpArrayShort);
}
const Array<Int>& RecordInterface::asArrayInt (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<Int>*)get_pointer (whichField, TpArrayInt);
}
const Array<uInt>& RecordInterface::asArrayuInt (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<uInt>*)get_pointer (whichField, TpArrayUInt);
}
const Array<Int64>& RecordInterface::asArrayInt64 (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<Int64>*)get_pointer (whichField, TpArrayInt64);
}
const Array<float>& RecordInterface::asArrayFloat (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<float>*)get_pointer (whichField, TpArrayFloat);
}
const Array<double>& RecordInterface::asArrayDouble (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<double>*)get_pointer (whichField, TpArrayDouble);
}
const Array<Complex>& RecordInterface::asArrayComplex (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<Complex>*)get_pointer (whichField, TpArrayComplex);
} 
const Array<DComplex>&  RecordInterface::asArrayDComplex (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<DComplex>*)get_pointer (whichField, TpArrayDComplex);
}
const Array<String>& RecordInterface::asArrayString (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Array<String>*)get_pointer (whichField, TpArrayString);
}




RecordNotice::RecordNotice (NoticeType changeType, uInt fieldNumber)
: changeType_p  (changeType),
  fieldNumber_p (fieldNumber)
{}

uInt RecordNotice::type() const
{
    // This function returns the "Notice" type, retrieved
    // from the "type registry".
    return Register(this);
}

int RecordNotice::operator== (const Notice& that) const
{
    if (type() != that.type()) {
	return 0;
    }
    return (changeType_p  == ((const RecordNotice&)that).changeType_p)
	&& (fieldNumber_p == ((const RecordNotice&)that).fieldNumber_p);
}

} //# NAMESPACE CASACORE - END

