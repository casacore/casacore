//# RecordRep.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2005
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

#include <casacore/casa/Containers/RecordRep.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstring>                  //# for memmove with gcc-4.3

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Tweaked for SUN NTV compiler.  Added the const_cast<void*> to give a hint to the Solaris compiler.


RecordRep::RecordRep ()
: nused_p (0)
{}
	
RecordRep::RecordRep (const RecordDesc& description)
: desc_p  (description),
  nused_p (0)
{
    restructure (desc_p, True);
}

RecordRep::RecordRep (const RecordRep& other)
: desc_p  (other.desc_p),
  nused_p (0)
{
    restructure (desc_p, False);
    copy_other (other);
}

RecordRep& RecordRep::operator= (const RecordRep& other)
{
    if (this != &other) {
	restructure (other.desc_p, False);
	copy_other (other);
    }
    return *this;
}

RecordRep::~RecordRep()
{
    delete_myself (desc_p.nfields());
}

void RecordRep::restructure (const RecordDesc& newDescription, Bool recursive)
{
    delete_myself (desc_p.nfields());
    desc_p  = newDescription;
    nused_p = desc_p.nfields();
    datavec_p.resize (nused_p);
    datavec_p = static_cast<void*>(0);
    data_p.resize (nused_p);
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    if (recursive) {
	        data_p[i] = new Record (this, desc_p.subRecord(i));
	    } else {
	        data_p[i] = new Record (this, RecordDesc());
	    }
	}else{
	    data_p[i] = createDataField (desc_p.type(i), desc_p.shape(i));
	}
    }
}

Int RecordRep::fieldNumber (const String& name) const
{
    return desc_p.fieldNumber (name);
}

void RecordRep::addDataPtr (void* ptr)
{
    if (nused_p >= data_p.nelements()) {
	datavec_p.resize (nused_p + 16);
	data_p.resize (nused_p + 16);
    }
    datavec_p[nused_p] = 0;
    data_p[nused_p++] = ptr;
}

void RecordRep::removeDataPtr (Int index)
{
    nused_p--;
    if (index < Int(nused_p)) {
	memmove (&datavec_p[index], &datavec_p[index+1],
		 (nused_p-index) * sizeof(void*));
	memmove (&data_p[index], &data_p[index+1],
		 (nused_p-index) * sizeof(void*));
    }
}

void RecordRep::removeData (Int whichField, void* ptr, void* vecptr)
{
    DataType type = desc_p.type(whichField);
    if (type == TpRecord) {
	delete static_cast<Record*>(ptr);
    }else{
	deleteDataField (type, ptr, vecptr);
    }
}

void RecordRep::removeField (Int whichField)
{
    removeData (whichField, data_p[whichField], datavec_p[whichField]);
    removeDataPtr (whichField);
    removeFieldFromDesc (whichField);
}

void RecordRep::addFieldToDesc (const String& name, DataType type,
				const IPosition& shape, Bool fixedShape)
{
    if (fixedShape) {
	desc_p.addField (name, type, shape);
    }else{
	desc_p.addField (name, type);
    }
}

void RecordRep::removeFieldFromDesc (Int whichField)
{
    desc_p.removeField (whichField);
}

void RecordRep::addDataField (const String& name, DataType type,
			      const IPosition& shape, Bool fixedShape,
			      const void* data)
{
    AlwaysAssert (type == TpBool      ||  type == TpArrayBool
              ||  type == TpUChar     ||  type == TpArrayUChar
              ||  type == TpShort     ||  type == TpArrayShort
              ||  type == TpInt       ||  type == TpArrayInt
              ||  type == TpUInt      ||  type == TpArrayUInt
              ||  type == TpInt64     ||  type == TpArrayInt64
              ||  type == TpFloat     ||  type == TpArrayFloat
              ||  type == TpDouble    ||  type == TpArrayDouble
              ||  type == TpComplex   ||  type == TpArrayComplex
              ||  type == TpDComplex  ||  type == TpArrayDComplex
              ||  type == TpString    ||  type == TpArrayString
                  , AipsError);
    addFieldToDesc (name, type, shape, fixedShape);
    void* ptr = createDataField (type, shape);
    copyDataField (type, ptr, data);
    addDataPtr (ptr);
}

void RecordRep::addField (const String& name, const Record& rec,
			  RecordInterface::RecordType type)
{
    // When the record is empty, it is variable structured.
    if (rec.nfields() == 0) {
	type = RecordInterface::Variable;
    }
    // When the new field is fixed, add its description too.
    if (type == RecordInterface::Fixed) {
	desc_p.addField (name, rec.description());
    }else{
	desc_p.addField (name, TpRecord);
    }
    // Use default ctor and assignment to be sure that the
    // new record gets the correct record type.
    Record* ptr = new Record (this, type);
    *ptr = rec;
    addDataPtr (ptr);
}

void RecordRep::checkShape (DataType type, const IPosition& shape,
			    const void* value, const String& fieldName)
{
    IPosition arrShape;
    switch (type) {
    case TpArrayBool:
	arrShape = static_cast<const Array<Bool>*>(value)->shape();
	break;
    case TpArrayUChar:
	arrShape = static_cast<const Array<uChar>*>(value)->shape();
	break;
    case TpArrayShort:
	arrShape = static_cast<const Array<Short>*>(value)->shape();
	break;
    case TpArrayInt:
	arrShape = static_cast<const Array<Int>*>(value)->shape();
	break;
    case TpArrayUInt:
	arrShape = static_cast<const Array<uInt>*>(value)->shape();
	break;
    case TpArrayInt64:
	arrShape = static_cast<const Array<Int64>*>(value)->shape();
	break;
    case TpArrayFloat:
	arrShape = static_cast<const Array<float>*>(value)->shape();
	break;
    case TpArrayDouble:
	arrShape = static_cast<const Array<double>*>(value)->shape();
	break;
    case TpArrayComplex:
	arrShape = static_cast<const Array<Complex>*>(value)->shape();
	break;
    case TpArrayDComplex:
	arrShape = static_cast<const Array<DComplex>*>(value)->shape();
	break;
    case TpArrayString:
	arrShape = static_cast<const Array<String>*>(value)->shape();
	break;
    default:
	throw (AipsError ("RecordRep::checkShape"));
    }
    if (! shape.isEqual (arrShape)) {
	throw (ArrayConformanceError
	       ("Record::define - fixed array conformance error for field " +
		fieldName));
    }
}

void RecordRep::defineDataField (Int whichField, DataType type,
				 const void* value)
{
    AlwaysAssert (whichField >= 0  &&  whichField < Int(nused_p), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
        if (type == TpRecord) {
	    *static_cast<Record*>(data_p[whichField]) =
	      *static_cast<const Record*>(value);
	}else{
	    if (desc_p.isArray(whichField)) {
	        const IPosition& shape = desc_p.shape(whichField);
		if (shape.nelements() > 0  &&  shape(0) > 0) {
		    checkShape (type, shape, value, desc_p.name(whichField));
		}
	    }
	    copyDataField (type, data_p[whichField], value);
	}
    } else if (isArray(type)  &&  asScalar(type) == descDtype) {
	// A scalar can be defined using a single element vector.
        checkShape (type, IPosition(1,1), value, desc_p.name(whichField));
	// Make sure there is a datavec entry.
	get_pointer (whichField, type);
	copyDataField (type, datavec_p[whichField], value);
    } else {
        throw (AipsError ("RecordRep::defineDataField - "
			  "incorrect data type used for field " +
			  desc_p.name(whichField)));
    }
}


void* RecordRep::createDataField (DataType type, const IPosition& shape)
{
    IPosition arrayShape;
    if (shape.nelements() > 0  &&  shape(0) > 0) {
	arrayShape = shape;
    }
    switch (type) {
    case TpBool:
	{
	    Bool* ptr = new Bool;
	    *ptr = False;
	    return ptr;
	}
    case TpUChar:
	{
	    uChar* ptr = new uChar;
	    *ptr = 0;
	    return ptr;
	}
    case TpShort:
	{
	    Short* ptr = new Short;
	    *ptr = 0;
	    return ptr;
	}
    case TpInt:
	{
	    Int* ptr = new Int;
	    *ptr = 0;
	    return ptr;
	}
    case TpUInt:
	{
	    uInt* ptr = new uInt;
	    *ptr = 0;
	    return ptr;
	}
    case TpInt64:
	{
	    Int64* ptr = new Int64;
	    *ptr = 0;
	    return ptr;
	}
    case TpFloat:
	{
	    float* ptr = new float;
	    *ptr = 0.0;
	    return ptr;
	}
    case TpDouble:
	{
	    double* ptr = new double;
	    *ptr = 0.0;
	    return ptr;
	}
    case TpComplex:
	return new Complex;
    case TpDComplex:
	return new DComplex;
    case TpString:
	return new String;
    case TpArrayBool:
	{
	    Array<Bool>* ptr = new Array<Bool> (arrayShape);
	    *ptr = False;
	    return ptr;
	}
    case TpArrayUChar:
	{
	    Array<uChar>* ptr = new Array<uChar> (arrayShape);
	    *ptr = 0;
	    return ptr;
	}
    case TpArrayShort:
	{
	    Array<Short>* ptr = new Array<Short> (arrayShape);
	    *ptr = 0;
	    return ptr;
	}
    case TpArrayInt:
	{
	    Array<Int>* ptr = new Array<Int> (arrayShape);
	    *ptr = 0;
	    return ptr;
	}
    case TpArrayUInt:
	{
	    Array<uInt>* ptr = new Array<uInt> (arrayShape);
	    *ptr = 0;
	    return ptr;
	}
    case TpArrayInt64:
	{
	    Array<Int64>* ptr = new Array<Int64> (arrayShape);
	    *ptr = 0;
	    return ptr;
	}
    case TpArrayFloat:
	{
	    Array<float>* ptr = new Array<float> (arrayShape);
	    *ptr = 0.0;
	    return ptr;
	}
    case TpArrayDouble:
	{
	    Array<double>* ptr = new Array<double> (arrayShape);
	    *ptr = 0.0;
	    return ptr;
	}
    case TpArrayComplex:
	return new Array<Complex> (arrayShape);
    case TpArrayDComplex:
	return new Array<DComplex> (arrayShape);
    case TpArrayString:
	return new Array<String> (arrayShape);
    default:
	throw (AipsError ("RecordRep::createDataField: unknown data type " +
                          String::toString(int(type))));
    }
}

void RecordRep::makeDataVec (Int whichField, DataType type)
{
    IPosition shape(1,1);
    switch (type) {
    case TpBool:
        datavec_p[whichField] = new Array<Bool>
	  (shape, static_cast<Bool*>(data_p[whichField]), SHARE);
	break;
    case TpUChar:
        datavec_p[whichField] = new Array<uChar>
	  (shape, static_cast<uChar*>(data_p[whichField]), SHARE);
	break;
    case TpShort:
        datavec_p[whichField] = new Array<Short>
	  (shape, static_cast<Short*>(data_p[whichField]), SHARE);
	break;
    case TpInt:
        datavec_p[whichField] = new Array<Int>
	  (shape, static_cast<Int*>(data_p[whichField]), SHARE);
	break;
    case TpUInt:
        datavec_p[whichField] = new Array<uInt>
	  (shape, static_cast<uInt*>(data_p[whichField]), SHARE);
	break;
    case TpInt64:
        datavec_p[whichField] = new Array<Int64>
	  (shape, static_cast<Int64*>(data_p[whichField]), SHARE);
	break;
    case TpFloat:
        datavec_p[whichField] = new Array<float>
	  (shape, static_cast<float*>(data_p[whichField]), SHARE);
	break;
    case TpDouble:
        datavec_p[whichField] = new Array<double>
	  (shape, static_cast<double*>(data_p[whichField]), SHARE);
	break;
    case TpComplex:
        datavec_p[whichField] = new Array<Complex>
	  (shape, static_cast<Complex*>(data_p[whichField]), SHARE);
	break;
    case TpDComplex:
        datavec_p[whichField] = new Array<DComplex>
	  (shape, static_cast<DComplex*>(data_p[whichField]), SHARE);
	break;
    case TpString:
        datavec_p[whichField] = new Array<String>
	  (shape, static_cast<String*>(data_p[whichField]), SHARE);
	break;
    default:
	throw (AipsError ("RecordRep::makeDataVec: unknown data type"));
    }
}

void RecordRep::delete_myself (uInt nfields)
{
    if (nfields > nused_p) {
	nfields = nused_p;
    }
    for (uInt i=0; i<nfields; i++) {
	removeData (i, data_p[i], datavec_p[i]);
	data_p[i] = 0;
	datavec_p[i] = 0;
    }
}

void RecordRep::deleteDataField (DataType type, void* ptr, void* vecptr)
{
    switch (type) {
    case TpBool:
	delete static_cast<Bool*>(ptr);
	delete static_cast<Array<Bool>*>(vecptr);
	break;
    case TpUChar:
	delete static_cast<uChar*>(ptr);
	delete static_cast<Array<uChar>*>(vecptr);
	break;
    case TpShort:
	delete static_cast<Short*>(ptr);
	delete static_cast<Array<Short>*>(vecptr);
	break;
    case TpInt:
	delete static_cast<Int*>(ptr);
	delete static_cast<Array<Int>*>(vecptr);
	break;
    case TpUInt:
	delete static_cast<uInt*>(ptr);
	delete static_cast<Array<uInt>*>(vecptr);
	break;
    case TpInt64:
	delete static_cast<Int64*>(ptr);
	delete static_cast<Array<Int64>*>(vecptr);
	break;
    case TpFloat:
	delete static_cast<float*>(ptr);
	delete static_cast<Array<float>*>(vecptr);
	break;
    case TpDouble:
	delete static_cast<double*>(ptr);
	delete static_cast<Array<double>*>(vecptr);
	break;
    case TpComplex:
	delete static_cast<Complex*>(ptr);
	delete static_cast<Array<Complex>*>(vecptr);
	break;
    case TpDComplex:
	delete static_cast<DComplex*>(ptr);
	delete static_cast<Array<DComplex>*>(vecptr);
	break;
    case TpString:
	delete static_cast<String*>(ptr);
	delete static_cast<Array<String>*>(vecptr);
	break;
    case TpArrayBool:
	delete static_cast<Array<Bool>*>(ptr);
	break;
    case TpArrayUChar:
	delete static_cast<Array<uChar>*>(ptr);
	break;
    case TpArrayShort:
	delete static_cast<Array<Short>*>(ptr);
	break;
    case TpArrayInt:
	delete static_cast<Array<Int>*>(ptr);
	break;
    case TpArrayUInt:
	delete static_cast<Array<uInt>*>(ptr);
	break;
    case TpArrayInt64:
	delete static_cast<Array<Int64>*>(ptr);
	break;
    case TpArrayFloat:
	delete static_cast<Array<float>*>(ptr);
	break;
    case TpArrayDouble:
	delete static_cast<Array<double>*>(ptr);
	break;
    case TpArrayComplex:
	delete static_cast<Array<Complex>*>(ptr);
	break;
    case TpArrayDComplex:
	delete static_cast<Array<DComplex>*>(ptr);
	break;
    case TpArrayString:
	delete static_cast<Array<String>*>(ptr);
	break;
    default:
	throw (AipsError ("RecordRep::deleteDataField"));
    }
}

Bool RecordRep::conform (const RecordRep& other) const
{
    // First check (non-recursively) if the descriptions conform.
    if (! desc_p.conform (other.desc_p)) {
	return False;
    }
    // Now check for each fixed sub-record if it conforms.
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    const Record& thisRecord = *static_cast<Record*>(const_cast<void*>(data_p[i]));
	    if (thisRecord.isFixed()) {
		const Record& thatRecord =
		  *static_cast<Record*>(const_cast<void*>(other.data_p[i]));
		if (! thisRecord.conform (thatRecord)) {
		    return False;
		}
	    }
	}
    }
    return True;
}

void RecordRep::copyData (const RecordRep& other)
{
    // Assume conform has already been called
    DebugAssert (conform (other), AipsError);
    copy_other (other);
}

void RecordRep::copy_other (const RecordRep& other)
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    *static_cast<Record*>(data_p[i]) =
	      *static_cast<Record*>(const_cast<void*>(other.data_p[i]));
	}else{
	    copyDataField (desc_p.type(i), data_p[i], other.data_p[i]);
	}
    }
}

void RecordRep::copyDataField (DataType type, Int whichField,
                               const void* that) const
{
    copyDataField (type, data_p[whichField], that);
}

void RecordRep::copyDataField (DataType type, void* ptr,
                               const void* that) const
{
    switch (type) {
    case TpBool:
	*static_cast<Bool*>(ptr) = *static_cast<const Bool*>(that);
	break;
    case TpUChar:
        *static_cast<uChar*>(ptr) = *static_cast<const uChar*>(that);
	break;
    case TpShort:
	*static_cast<Short*>(ptr) = *static_cast<const Short*>(that);
	break;
    case TpInt:
	*static_cast<Int*>(ptr) = *static_cast<const Int*>(that);
	break;
    case TpUInt:
	*static_cast<uInt*>(ptr) = *static_cast<const uInt*>(that);
	break;
    case TpInt64:
	*static_cast<Int64*>(ptr) = *static_cast<const Int64*>(that);
	break;
    case TpFloat:
	*static_cast<float*>(ptr) = *static_cast<const float*>(that);
	break;
    case TpDouble:
	*static_cast<double*>(ptr) = *static_cast<const double*>(that);
	break;
    case TpComplex:
	*static_cast<Complex*>(ptr) = *static_cast<const Complex*>(that);
	break;
    case TpDComplex:
	*static_cast<DComplex*>(ptr) = *static_cast<const DComplex*>(that);
	break;
    case TpString:
	*static_cast<String*>(ptr) = *static_cast<const String*>(that);
	break;
    case TpArrayBool:
        static_cast<Array<Bool>*>(ptr)->resize
	  (static_cast<const Array<Bool>*>(that)->shape());
	*static_cast<Array<Bool>*>(ptr) =
	  *static_cast<const Array<Bool>*>(that);
	break;
    case TpArrayUChar:
	static_cast<Array<uChar>*>(ptr)->resize
	  (static_cast<const Array<uChar>*>(that)->shape());
	*static_cast<Array<uChar>*>(ptr) =
	  *static_cast<const Array<uChar>*>(that);
	break;
    case TpArrayShort:
	static_cast<Array<Short>*>(ptr)->resize
	  (static_cast<const Array<Short>*>(that)->shape());
	*static_cast<Array<Short>*>(ptr) =
	  *static_cast<const Array<Short>*>(that);
	break;
    case TpArrayInt:
	static_cast<Array<Int>*>(ptr)->resize
	  (static_cast<const Array<Int>*>(that)->shape());
	*static_cast<Array<Int>*>(ptr) =
	  *static_cast<const Array<Int>*>(that);
	break;
    case TpArrayUInt:
	static_cast<Array<uInt>*>(ptr)->resize
	  (static_cast<const Array<uInt>*>(that)->shape());
	*static_cast<Array<uInt>*>(ptr) =
	  *static_cast<const Array<uInt>*>(that);
	break;
    case TpArrayInt64:
	static_cast<Array<Int64>*>(ptr)->resize
	  (static_cast<const Array<Int64>*>(that)->shape());
	*static_cast<Array<Int64>*>(ptr) 
	  = *static_cast<const Array<Int64>*>(that);
	break;
    case TpArrayFloat:
	static_cast<Array<float>*>(ptr)->resize
	  (static_cast<const Array<float>*>(that)->shape());
	*static_cast<Array<float>*>(ptr) 
	  = *static_cast<const Array<float>*>(that);
	break;
    case TpArrayDouble:
	static_cast<Array<double>*>(ptr)->resize
	  (static_cast<const Array<double>*>(that)->shape());
	*static_cast<Array<double>*>(ptr) =
	  *static_cast<const Array<double>*>(that);
	break;
    case TpArrayComplex:
	static_cast<Array<Complex>*>(ptr)->resize
	  (static_cast<const Array<Complex>*>(that)->shape());
	*static_cast<Array<Complex>*>(ptr) =
	  *static_cast<const Array<Complex>*>(that);
	break;
    case TpArrayDComplex:
	static_cast<Array<DComplex>*>(ptr)->resize
	  (static_cast<const Array<DComplex>*>(that)->shape());
	*static_cast<Array<DComplex>*>(ptr) =
	  *static_cast<const Array<DComplex>*>(that);
	break;
    case TpArrayString:
        static_cast<Array<String>*>(ptr)->resize
	  (static_cast<const Array<String>*>(that)->shape());
	*static_cast<Array<String>*>(ptr) =
	  *static_cast<const Array<String>*>(that);
	break;
    default:
	throw (AipsError ("RecordRep::copyDataField"));
    }
}


void* RecordRep::get_pointer (Int whichField, DataType type,
			      const String& recordType) const
{
    AlwaysAssert (recordType == "Record", AipsError);
    return get_pointer (whichField, type);
}
void* RecordRep::get_pointer (Int whichField, DataType type) const
{
    AlwaysAssert (whichField >= 0  &&  whichField < Int(nused_p), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
        return data_p[whichField];
    }
    // A scalar can be returned as an array.
    if (! (isArray(type)  &&  asScalar(type) == descDtype)) {
        throw (AipsError ("RecordRep::get_pointer - "
			  "incorrect data type used for field " +
			  desc_p.name(whichField)));
    }
    if (datavec_p[whichField] == 0) {
        const_cast<RecordRep*>(this)->makeDataVec (whichField, descDtype);
    }
    return datavec_p[whichField];
}



void RecordRep::mergeField (const RecordRep& other, Int whichFieldFromOther,
			    RecordInterface::DuplicatesFlag flag)
{
    // If the field exists and if flag tells to overwrite,
    // the field is removed first.
    if (flag == RecordInterface::OverwriteDuplicates) {
	Int fld = desc_p.fieldNumber (other.desc_p.name(whichFieldFromOther));
	if (fld >= 0) {
	    removeField (fld);
	}
    }
    // Try to add the field to the description.
    Int nr = desc_p.nfields();
    Int nrnew = desc_p.mergeField (other.desc_p, whichFieldFromOther, flag);
    // It succeeded if nfields increased.
    // Then the value can be defined.
    if (nrnew > nr) {
	DataType type = desc_p.type (nr);
	void* otherPtr = other.get_pointer (whichFieldFromOther, type);
	void* ptr;
	if (type == TpRecord) {
	    ptr = new Record (*static_cast<Record*>(otherPtr));
	}else{
	    ptr = createDataField (type, desc_p.shape(nr));
	    copyDataField (type, ptr, otherPtr);
	}
	addDataPtr (ptr);
    }
}

void RecordRep::merge (const RecordRep& other,
		       RecordInterface::DuplicatesFlag flag)
{
    Int n = other.desc_p.nfields();
    for (Int i=0; i<n; i++) {
	mergeField (other, i, flag);
    }
}

    
void RecordRep::printDataField (std::ostream& os, DataType type,
				const String& indent, Int maxNrValues,
				const void* ptr) const
{
    switch (type) {
    case TpBool:
        os << "Bool " << *static_cast<const Bool*>(ptr);
	break;
    case TpUChar:
        os << "uChar " << Int(*static_cast<const uChar*>(ptr));
	break;
    case TpShort:
	os << "Short " << *static_cast<const Short*>(ptr);
	break;
    case TpInt:
	os << "Int " << *static_cast<const Int*>(ptr);
	break;
    case TpUInt:
	os << "uInt " << *static_cast<const uInt*>(ptr);
	break;
    case TpInt64:
	os << "Int64 " << *static_cast<const Int64*>(ptr);
	break;
    case TpFloat:
	os << "Float " << *static_cast<const float*>(ptr);
	break;
    case TpDouble:
	os << "Double " << *static_cast<const double*>(ptr);
	break;
    case TpComplex:
	os << "Complex " << *static_cast<const Complex*>(ptr);
	break;
    case TpDComplex:
	os << "DComplex " << *static_cast<const DComplex*>(ptr);
	break;
    case TpString:
        os << "String " << '"' << *static_cast<const String*>(ptr) << '"';
	break;
    case TpArrayBool:
        os << "Bool array with shape "
	   << static_cast<const Array<Bool>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Bool>& arr = *static_cast<const Array<Bool>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Bool> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayUChar:
        os << "uChar array with shape "
	   << static_cast<const Array<uChar>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<uChar>& arr = *static_cast<const Array<uChar>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<uChar> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayShort:
        os << "Short array with shape "
	   << static_cast<const Array<Short>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Short>& arr = *static_cast<const Array<Short>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Short> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayInt:
        os << "Int array with shape "
	   << static_cast<const Array<Int>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Int>& arr = *static_cast<const Array<Int>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Int> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayUInt:
        os << "uInt array with shape "
	   << static_cast<const Array<uInt>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<uInt>& arr = *static_cast<const Array<uInt>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<uInt> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayInt64:
        os << "Int64 array with shape "
	   << static_cast<const Array<Int64>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Int64>& arr = *static_cast<const Array<Int64>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Int64> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayFloat:
        os << "Float array with shape "
	   << static_cast<const Array<Float>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Float>& arr = *static_cast<const Array<Float>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Float> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayDouble:
        os << "Double array with shape "
	   << static_cast<const Array<Double>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Double>& arr = *static_cast<const Array<Double>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Double> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayComplex:
        os << "Complex array with shape "
	   << static_cast<const Array<Complex>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<Complex>& arr = *static_cast<const Array<Complex>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<Complex> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayDComplex:
        os << "DComplex array with shape "
	   << static_cast<const Array<DComplex>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<DComplex>& arr = *static_cast<const Array<DComplex>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<DComplex> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    case TpArrayString:
        os << "String array with shape "
	   << static_cast<const Array<String>*>(ptr)->shape();
	if (maxNrValues != 0) {
	  const Array<String>& arr = *static_cast<const Array<String>*>(ptr);
	  if (maxNrValues < 0) {
	    os << endl << arr;
	  } else {
	    Vector<String> vec = arr.reform (IPosition(1, arr.nelements()));
	    if (uInt(maxNrValues+1) >= vec.nelements()) {
	      os << endl << indent << "  " << vec;
	    } else {
	      os << ", first values:"
		 << endl << indent << "  "
		 << vec(Slice(0,maxNrValues-1));
	    }
	  }
	}
	break;
    default:
	throw (AipsError ("RecordRep::printDataField"));
    }
}

void RecordRep::print (std::ostream& os,
		       Int maxNrValues, const String& indent) const
{
    for (uInt i=0; i<nused_p; i++) {
        os << indent << desc_p.name(i) << ": ";
	if (desc_p.type(i) == TpRecord) {
	    os << '{' << endl;
	    static_cast<const Record*>(data_p[i])->print (os, maxNrValues,
							  indent+"  ");
	    os << indent << '}' << endl;
        } else {
	    printDataField (os, desc_p.type(i),
			    indent, maxNrValues, data_p[i]);
	    os << endl;
	}
    }
}


void RecordRep::putDataField (AipsIO& os, DataType type, const void* ptr) const
{
    switch (type) {
    case TpBool:
	os << *static_cast<const Bool*>(ptr);
	break;
    case TpUChar:
	os << *static_cast<const uChar*>(ptr);
	break;
    case TpShort:
	os << *static_cast<const Short*>(ptr);
	break;
    case TpInt:
	os << *static_cast<const Int*>(ptr);
	break;
    case TpUInt:
	os << *static_cast<const uInt*>(ptr);
	break;
    case TpInt64:
	os << *static_cast<const Int64*>(ptr);
	break;
    case TpFloat:
	os << *static_cast<const float*>(ptr);
	break;
    case TpDouble:
	os << *static_cast<const double*>(ptr);
	break;
    case TpComplex:
	os << *static_cast<const Complex*>(ptr);
	break;
    case TpDComplex:
	os << *static_cast<const DComplex*>(ptr);
	break;
    case TpString:
	os << *static_cast<const String*>(ptr);
	break;
    case TpArrayBool:
	putArray (os, *static_cast<const Array<Bool>*>(ptr), "Array<void>");
	break;
    case TpArrayUChar:
	putArray (os, *static_cast<const Array<uChar>*>(ptr), "Array<uChar>");
	break;
    case TpArrayShort:
	putArray (os, *static_cast<const Array<Short>*>(ptr), "Array<short>");
	break;
    case TpArrayInt:
	putArray (os, *static_cast<const Array<Int>*>(ptr), "Array<Int>");
	break;
    case TpArrayUInt:
	putArray (os, *static_cast<const Array<uInt>*>(ptr), "Array<uInt>");
	break;
    case TpArrayInt64:
	putArray (os, *static_cast<const Array<Int64>*>(ptr),
		  "Array<Int64>");
	break;
    case TpArrayFloat:
	putArray (os, *static_cast<const Array<float>*>(ptr),
		  "Array<float>");
	break;
    case TpArrayDouble:
	putArray (os, *static_cast<const Array<double>*>(ptr),
		  "Array<double>");
	break;
    case TpArrayComplex:
	putArray (os, *static_cast<const Array<Complex>*>(ptr),
		  "Array<void>");
	break;
    case TpArrayDComplex:
	putArray (os, *static_cast<const Array<DComplex>*>(ptr),
		  "Array<void>");
	break;
    case TpArrayString:
	putArray (os, *static_cast<const Array<String>*>(ptr),
		  "Array<String>");
	break;
    default:
	throw (AipsError ("RecordRep::putDataField"));
    }
}

void RecordRep::getDataField (AipsIO& os, DataType type, void* ptr)
{
    switch (type) {
    case TpBool:
	os >> *static_cast<Bool*>(ptr);
	break;
    case TpUChar:
	os >> *static_cast<uChar*>(ptr);
	break;
    case TpShort:
	os >> *static_cast<Short*>(ptr);
	break;
    case TpInt:
	os >> *static_cast<Int*>(ptr);
	break;
    case TpUInt:
	os >> *static_cast<uInt*>(ptr);
	break;
    case TpInt64:
	os >> *static_cast<Int64*>(ptr);
	break;
    case TpFloat:
	os >> *static_cast<float*>(ptr);
	break;
    case TpDouble:
	os >> *static_cast<double*>(ptr);
	break;
    case TpComplex:
	os >> *static_cast<Complex*>(ptr);
	break;
    case TpDComplex:
	os >> *static_cast<DComplex*>(ptr);
	break;
    case TpString:
	os >> *static_cast<String*>(ptr);
	break;
    case TpArrayBool:
	os >> *static_cast<Array<Bool>*>(ptr);
	break;
    case TpArrayUChar:
	os >> *static_cast<Array<uChar>*>(ptr);
	break;
    case TpArrayShort:
	os >> *static_cast<Array<Short>*>(ptr);
	break;
    case TpArrayInt:
	os >> *static_cast<Array<Int>*>(ptr);
	break;
    case TpArrayUInt:
	os >> *static_cast<Array<uInt>*>(ptr);
	break;
    case TpArrayInt64:
	os >> *static_cast<Array<Int64>*>(ptr);
	break;
    case TpArrayFloat:
	os >> *static_cast<Array<float>*>(ptr);
	break;
    case TpArrayDouble:
	os >> *static_cast<Array<double>*>(ptr);
	break;
    case TpArrayComplex:
	os >> *static_cast<Array<Complex>*>(ptr);
	break;
    case TpArrayDComplex:
	os >> *static_cast<Array<DComplex>*>(ptr);
	break;
    case TpArrayString:
	os >> *static_cast<Array<String>*>(ptr);
	break;
    default:
	throw (AipsError ("RecordRep::getDataField"));
    }
}

void RecordRep::putRecord (AipsIO& os, int recordType) const
{
    os.putstart ("Record", 1);              // version 1
    os << desc_p;
    os << recordType;
    putData (os);
    os.putend();
}

void RecordRep::putData (AipsIO& os) const
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		os << *static_cast<Record*>(const_cast<void*>(data_p[i]));
	    }else{
		static_cast<Record*>(const_cast<void*>(data_p[i]))->putData (os);
	    }
	}else{
	    putDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void RecordRep::getRecord (AipsIO& os, Int& recordType)
{
    // Support reading scalar and array keyword sets as records.
    // They are the very old way of storing keywords, since long replaced
    // by Record. The code does not exist anymore, but theoretically such data
    // can exist in a very old table. Therefore it is still supported here.
    uInt version;
    String type = os.getNextType();
    if (type == "ScalarKeywordSet") {
	version = os.getstart ("ScalarKeywordSet");
	getKeySet (os, version, 0);
    } else if (type == "ArrayKeywordSet") {
	version = os.getstart ("ArrayKeywordSet");
	getKeySet (os, version, 1);
    }else{
	uInt version = os.getstart ("Record");
	// Get the description and restructure the record.
	RecordDesc desc;
	os >> desc;
	os >> recordType;
	restructure (desc, True);
	// Read the data.
	getData (os, version);
    }
    os.getend();
}

void RecordRep::getData (AipsIO& os, uInt version)
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		os >> *static_cast<Record*>(data_p[i]);
	    }else{
		static_cast<Record*>(data_p[i])->getData (os, version);
	    }
	}else{
	    getDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void RecordRep::getKeySet (AipsIO& os, uInt version, uInt type)
{
    // First build the description from the map of keyword names and
    // attributes.
    RecordDesc desc;
    getKeyDesc (os, desc);
    // Define the record from the description.
    // Read the keyword values and define the corresponding record value.
    restructure (desc, True);
    getScalarKeys (os);
    if (type == 1) {
	getArrayKeys (os);
    }
    // Newer keyword sets may contain nested keyword sets.
    // We do not support reading those, so throw an exception when they exist.
    if (version > 1) {
	uInt n;
	os >> n;
	AlwaysAssert (n==0, AipsError);
    }
}

void RecordRep::getKeyDesc (AipsIO& os, RecordDesc& desc)
{
    // Start reading the Map of keyword names and attributes.
    os.getstart ("Map<String,void>");
    int dt;
    String name, comment;
    // Get #names and the default attribute (datatype + comment).
    uInt i, n;
    os >> n;
    os >> dt;
    os >> comment;
    // Get each keyword name and attribute.
    // Add them to the record description.
    for (i=0; i<n; i++) {
	os >> name;
	os >> dt;
	os >> comment;
	desc.addField (name, DataType(dt));
    }
    os.getend();
    // Get the excluded data types and names.
    // Note that exNames was written as a Block<Regex>, but can be
    // read as a Block<String>. This is a template instantiation less.
    Block<int>    exDtype;
    Block<String> exNames;
    os >> exDtype;
    os >> exNames;
}

void RecordRep::getScalarKeys (AipsIO& os)
{
    uInt i, n;
    String name;
    // Read the values per type.
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpBool, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpInt, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpUInt, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpFloat, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpDouble, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpComplex, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpDComplex, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpString, data_p[fieldNumber (name)]);
    }
}

void RecordRep::getArrayKeys (AipsIO& os)
{
    uInt i, n;
    String name;
    // Read the values per type.
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayBool, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayInt, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayUInt, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayFloat, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayDouble, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayComplex, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayDComplex, data_p[fieldNumber (name)]);
    }
    os >> n;
    for (i=0; i<n; i++) {
	os >> name;
	getDataField (os, TpArrayString, data_p[fieldNumber (name)]);
    }
}

} //# NAMESPACE CASACORE - END

