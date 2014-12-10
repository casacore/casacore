//# RecordDescRep.cc: Description of the fields in a Record object
//# Copyright (C) 1996,1997,1998,2001,2002
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

#include <casacore/casa/Containers/RecordDescRep.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/stdio.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

RecordDescRep::RecordDescRep()
: n_p(0),
  types_p(0),
  names_p(0),
  sub_records_p(0),
  shapes_p(0),
  is_array_p(0),
  tableDescNames_p(0),
  comments_p(0),
  name_map_p(-1, 64)
{
    // Nothing
}

RecordDescRep::RecordDescRep (const RecordDescRep& other)
: n_p(0),
  types_p(0),
  names_p(0),
  sub_records_p(0),
  shapes_p(0),
  is_array_p(0),
  tableDescNames_p(0),
  comments_p(0),
  name_map_p(-1, 64)
{
    copy_other (other);
}

RecordDescRep& RecordDescRep::operator= (const RecordDescRep& other)
{
    if (this != &other) {
	copy_other (other);
    }
    return *this;
}

RecordDescRep::~RecordDescRep()
{
    for (uInt i=0; i<n_p; i++) {
	if (sub_records_p[i]) {
	    delete sub_records_p[i];
	    sub_records_p[i] = 0;
	}
    }
}

void RecordDescRep::addFieldName (const String& fieldName, DataType type)
{
    if (fieldNumber(fieldName) >= 0) {
	throw (AipsError ("RecordDesc::addField() - field " +
			  fieldName + " already has been defined"));
    }
    increment_length();
    uInt n = n_p - 1;
    types_p[n] = type;
    names_p[n] = fieldName;
    name_map_p.define (fieldName, n);
    sub_records_p[n] = 0;
    is_array_p[n] = False;
    shapes_p[n].resize(1);
    shapes_p[n] = IPosition(1,1);
}

uInt RecordDescRep::addField (const String& fieldName, DataType type)
{
    addFieldName (fieldName, type);
    if (type == TpRecord) {
	sub_records_p[n_p - 1] = new RecordDesc;
    } else if (type != TpTable) {
	addFieldAny  (type);
    }
    return n_p;
}
void RecordDescRep::addFieldAny (DataType type)
{
    uInt n = n_p - 1;
    switch(type) {
    case TpBool:
    case TpChar:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpInt64:
    case TpFloat:
    case TpDouble:
    case TpComplex:
    case TpDComplex:
    case TpString:
	break;
    case TpArrayBool:
    case TpArrayChar:
    case TpArrayUChar:
    case TpArrayShort:
    case TpArrayUShort:
    case TpArrayInt:
    case TpArrayUInt:
    case TpArrayInt64:
    case TpArrayFloat:
    case TpArrayDouble:
    case TpArrayComplex:
    case TpArrayDComplex:
    case TpArrayString:
	shapes_p[n] = IPosition(1,-1);
	is_array_p[n] = True;
	break;
    default:
	removeField (n);
	throw (AipsError ("RecordDesc::addField(const String& fieldName, "
			  "DataType type) - unknown datatype"));
    }
}

uInt RecordDescRep::addArray (const String& fieldName, DataType type,
			      const IPosition& shape)
{
    addFieldName  (fieldName, type);
    addFieldArray (type, shape);
    return n_p;
}
void RecordDescRep::addFieldArray (DataType type, const IPosition& shape)
{
    uInt n = n_p - 1;
    shapes_p[n].resize(shape.nelements());
    shapes_p[n] = shape;
    is_array_p[n] = True;
    
    switch(type) {
    case TpBool:
    case TpArrayBool:
	types_p[n] = TpArrayBool;
	break;
    case TpChar:
    case TpArrayChar:
	types_p[n] = TpArrayChar;
	break;
    case TpUChar:
    case TpArrayUChar:
	types_p[n] = TpArrayUChar;
	break;
    case TpShort:
    case TpArrayShort:
	types_p[n] = TpArrayShort;
	break;
    case TpUShort:
    case TpArrayUShort:
	types_p[n] = TpArrayUShort;
	break;
    case TpInt:
    case TpArrayInt:
	types_p[n] = TpArrayInt;
	break;
    case TpUInt:
    case TpArrayUInt:
	types_p[n] = TpArrayUInt;
	break;
    case TpInt64:
    case TpArrayInt64:
	types_p[n] = TpArrayInt64;
	break;
    case TpFloat:
    case TpArrayFloat:
	types_p[n] = TpArrayFloat;
	break;
    case TpDouble:
    case TpArrayDouble:
	types_p[n] = TpArrayDouble;
	break;
    case TpComplex:
    case TpArrayComplex:
	types_p[n] = TpArrayComplex;
	break;
    case TpDComplex:
    case TpArrayDComplex:
	types_p[n] = TpArrayDComplex;
	break;
    case TpString:
    case TpArrayString:
	types_p[n] = TpArrayString;
	break;
    default:
	removeField (n);
	throw (AipsError ("RecordDesc::addField(const String& fieldName, "
			  "DataType type) - unknown datatype (must be array"
			  "or ordinary scalar type)"));
    }
}

uInt RecordDescRep::addRecord (const String& fieldName, 
			       const RecordDesc& subDesc)
{
    addFieldName (fieldName, TpRecord);
    sub_records_p[n_p - 1] = new RecordDesc(subDesc);
    AlwaysAssert (sub_records_p[n_p - 1] != 0, AipsError);
    return n_p;
}

uInt RecordDescRep::addTable (const String& fieldName, 
			      const String& tableDescName)
{
    addFieldName (fieldName, TpTable);
    tableDescNames_p[n_p - 1] = tableDescName;
    return n_p;
}


const String& RecordDescRep::comment (Int whichField) const
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    return comments_p[whichField];
}

void RecordDescRep::setComment (Int whichField, const String& comment)
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    comments_p[whichField] = comment;
}

void RecordDescRep::setShape (Int whichField, const IPosition& shape)
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    AlwaysAssert (isArray(whichField), AipsError);
    shapes_p[whichField] = shape;
}


uInt RecordDescRep::mergeField (const RecordDescRep& other,
				Int whichField,
				int duplicateAction)
{
    AlwaysAssert (whichField>=0 && whichField < Int(other.nfields()),
		  AipsError);
    String newName = other.name (whichField);
    Int duplicateNumber = fieldNumber (newName);
    if (duplicateNumber >= 0) {
	switch (duplicateAction) {
	case RecordInterface::SkipDuplicates:
	    return nfields();
	case RecordInterface::ThrowOnDuplicates:
	    throw (AipsError ("RecordDesc::mergeField - duplicate in other"));
	case RecordInterface::OverwriteDuplicates:
	    removeField (duplicateNumber);     // We'll add it back below
	    break;
	case RecordInterface::RenameDuplicates:
	    newName = uniqueName (newName);
	    break;
	default:
	    AlwaysAssert ((0), AipsError); // NOTREACHED
	}
    }
    addRepField (other, newName, whichField);
    return nfields();
}

void RecordDescRep::addRepField (const RecordDescRep& other, 
				 const String& newName,
				 Int whichField)
{
    if (other.isScalar (whichField)) {
	addField (newName, other.type(whichField));
    } else if (other.isArray (whichField)) {
	addArray (newName, other.type(whichField), other.shape(whichField));
    } else if (other.isTable (whichField)) {
	addTable (newName, other.tableDescName(whichField));
    } else if (other.isSubRecord (whichField)) {
	addRecord (newName, other.subRecord(whichField));
    } else {
	AlwaysAssert (0, AipsError); // NOTREACHED
    }
    comments_p[n_p-1] = other.comment (whichField);
}

uInt RecordDescRep::merge (const RecordDescRep& other, int duplicateAction)
{
    for (uInt i=0; i < other.nfields(); i++) {
	mergeField (other, i, duplicateAction);
    }
    return nfields();
}

uInt RecordDescRep::removeField (Int whichField)
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    if (sub_records_p[whichField]) {
	delete sub_records_p[whichField];
	sub_records_p[whichField] = 0;
    }
    n_p--;
    // Remove the field from the name map.
    // Decrement the field number of all fields following it.
    name_map_p.remove (names_p[whichField]);
    for (uInt i=0; i<n_p; i++) {
	Int& inx = name_map_p.getVal (i);
	if (inx > whichField) {
	    inx--;
	}
    }
    types_p.remove (whichField);
    names_p.remove (whichField);
    sub_records_p.remove (whichField);
    shapes_p.remove (whichField);
    is_array_p.remove (whichField);
    tableDescNames_p.remove (whichField);
    comments_p.remove (whichField);
    return n_p;
}

void RecordDescRep::renameField (const String& newName, Int whichField)
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    name_map_p.rename (newName, names_p[whichField]);
    names_p[whichField] = newName;
}

void RecordDescRep::setShape (const IPosition& shape, Int whichField)
{
    AlwaysAssert (whichField>=0 && whichField < Int(n_p), AipsError);
    shapes_p[whichField].resize (shape.nelements());
    shapes_p[whichField] = shape;
}

Int RecordDescRep::fieldNumber (const String& fieldName) const
{
    const Int* inx = name_map_p.isDefined (fieldName);
    return (inx == 0  ?  -1 : *inx);
}

String RecordDescRep::makeName (Int whichField) const
{
    char strc[8];
    sprintf(strc, "*%i", whichField+1);
    return uniqueName (strc);
}

String RecordDescRep::uniqueName (const String& name) const
{
    String newName = name;
    char strc[8];      // should be plenty large...
    int n = 0;
    while (fieldNumber(newName) >= 0) {
	++n;
	sprintf(strc, "_%i", n);
	newName = name + strc;
    }
    return newName;
}

RecordDesc& RecordDescRep::subRecord (Int whichField)
{
    AlwaysAssert (isSubRecord(whichField), AipsError);
    return *sub_records_p[whichField];
}

Bool RecordDescRep::conform (const RecordDescRep& other) const
{
    if (this == &other) {
	return True;
    }
    uInt n = nfields();
    if (n != other.nfields()) {
	return False;
    }

    for (uInt i=0; i < n; i++) {
	if (type(i) != other.type(i)) {
	    return False;
	}
	if (! shapes_p[i].isEqual (other.shapes_p[i])) {
	    return False;
	}
	if (tableDescNames_p[i] != tableDescNames_p[i]) {
	    return False;
	}
	if (sub_records_p[i]) {
	    if (! other.sub_records_p[i]) {
		return False;
	    }
	}
    }
    return True;
}

Bool RecordDescRep::operator== (const RecordDescRep& other) const
{
    if (this == &other) {
	return True;
    }
    if (!conform (other)) {
	return False;
    }
    // Now check recursively if the sub-records conform.
    uInt n = nfields();
    for (uInt i=0; i<n; i++) {
	if (sub_records_p[i]) {
	    if (subRecord(i) != other.subRecord(i)) {
		return False;
	    }
	}
    }
    return True;
}

Bool RecordDescRep::operator!= (const RecordDescRep& other) const
{
    return (! ((*this) == other));
}

Bool RecordDescRep::isEqual (const RecordDescRep& other,
			     Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (nfields() != other.nfields()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool RecordDescRep::isSubset (const RecordDescRep& other,
			      Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (nfields() > other.nfields()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool RecordDescRep::isStrictSubset (const RecordDescRep& other,
				    Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (nfields() >= other.nfields()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool RecordDescRep::allExist (const RecordDescRep& other,
			      Bool& equalDataTypes) const
{
    equalDataTypes = True;
    uInt n = nfields();
    for (uInt i=0; i<n; i++) {
	Int whichField = other.fieldNumber (names_p[i]);
	if (whichField < 0) {
	    return False;                     // name does not exist in other
	}
	if (type(i) != other.type(i)) {
	    equalDataTypes = False;           // unequal data type
	}
    }
    return True;                              // keyword names are equal
}

Bool RecordDescRep::isDisjoint (const RecordDescRep& other) const
{
    uInt n = nfields();
    for (uInt i=0; i<n; i++) {
	if (other.fieldNumber (names_p[i]) >= 0) {
	    return False;                     // name exists in other
	}
    }
    return True;
}


void RecordDescRep::copy_other (const RecordDescRep& other)
{
    uInt i;
    // First, we need to free up the storage of any extant sub records
    for (i=0; i < n_p ; i++) {
	if (sub_records_p[i]) {
	    delete sub_records_p[i];
	    sub_records_p[i] = 0;
	}
    }
    // Then copy
    n_p = other.n_p;
    types_p = other.types_p;
    names_p = other.names_p;
    name_map_p = other.name_map_p;
    shapes_p = other.shapes_p;
    is_array_p = other.is_array_p;
    tableDescNames_p = other.tableDescNames_p;
    comments_p = other.comments_p;
    sub_records_p = other.sub_records_p;
    // Now clone them. As an alternative, we could have used a counted pointer.
    for (i=0; i < n_p; i++) {
	if (sub_records_p[i]) {
	    sub_records_p[i] = new RecordDesc(*sub_records_p[i]);
	    AlwaysAssert(sub_records_p[i] != 0, AipsError);
	}
    }
}

void RecordDescRep::increment_length()
{
    n_p++;
    if (n_p > types_p.nelements()) {
	uInt newSize = 2*n_p;
	types_p.resize (newSize);
	names_p.resize (newSize);
	shapes_p.resize (newSize);
	sub_records_p.resize (newSize);
	is_array_p.resize (newSize);
	tableDescNames_p.resize (newSize);
	comments_p.resize (newSize);
	// This is to shut up tools that note when you read an unset
	// value.
	IPosition scalarShape(1,1);
	for (uInt i=n_p; i < types_p.nelements(); i++) {
	    types_p[i] = 0;
	    sub_records_p[i] = 0;
	    is_array_p[i] = False;
	    shapes_p[i].resize (scalarShape.nelements());
	    shapes_p[i] = scalarShape;
	    // names_p, etc. is already set since the default ctor is called
	    // for all its elements when new String[] is called. The
	    // joys of types with constructors vs. built-in types...
	}
    }
}

} //# NAMESPACE CASACORE - END

