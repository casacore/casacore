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

#include <casacore/casa/Containers/RecordDescRep.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/stdio.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


void RecordDescRep::Data::copy (const RecordDescRep::Data& that)
{
    type_p = that.type_p;
    name_p = that.name_p;
    shape_p = that.shape_p;
    is_array_p = that.is_array_p;
    tableDescName_p = that.tableDescName_p;
    comment_p = that.comment_p;
    if (that.sub_record_p) {
        sub_record_p.reset (new RecordDesc(*that.sub_record_p));
    } else {
        sub_record_p.reset();
    }
}


void RecordDescRep::addFieldName (const String& fieldName, DataType type)
{
    if (fieldNumber(fieldName) >= 0) {
	throw (AipsError ("RecordDesc::addField() - field " +
			  fieldName + " already has been defined"));
    }
    data_p.emplace_back (fieldName, type);
    name_map_p.insert (std::make_pair(fieldName, data_p.size()-1));
}

uInt RecordDescRep::addField (const String& fieldName, DataType type)
{
    addFieldName (fieldName, type);
    if (type == TpRecord) {
      data_p.back().sub_record_p.reset (new RecordDesc);
    } else if (type != TpTable) {
	addFieldAny (type);
    }
    return nfields();
}
void RecordDescRep::addFieldAny (DataType type)
{
    uInt n = data_p.size() - 1;
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
        data_p.back().setArrayShape (IPosition(1,-1));
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
    return nfields();
}
void RecordDescRep::addFieldArray (DataType type, const IPosition& shape)
{
    data_p.back().setArrayShape (shape);
    switch(type) {
    case TpBool:
    case TpArrayBool:
        data_p.back().type_p = TpArrayBool;
        break;
    case TpChar:
    case TpArrayChar:
	data_p.back().type_p = TpArrayChar;
	break;
    case TpUChar:
    case TpArrayUChar:
	data_p.back().type_p = TpArrayUChar;
	break;
    case TpShort:
    case TpArrayShort:
	data_p.back().type_p = TpArrayShort;
	break;
    case TpUShort:
    case TpArrayUShort:
	data_p.back().type_p = TpArrayUShort;
	break;
    case TpInt:
    case TpArrayInt:
	data_p.back().type_p = TpArrayInt;
	break;
    case TpUInt:
    case TpArrayUInt:
	data_p.back().type_p = TpArrayUInt;
	break;
    case TpInt64:
    case TpArrayInt64:
	data_p.back().type_p = TpArrayInt64;
	break;
    case TpFloat:
    case TpArrayFloat:
	data_p.back().type_p = TpArrayFloat;
	break;
    case TpDouble:
    case TpArrayDouble:
	data_p.back().type_p = TpArrayDouble;
	break;
    case TpComplex:
    case TpArrayComplex:
	data_p.back().type_p = TpArrayComplex;
	break;
    case TpDComplex:
    case TpArrayDComplex:
	data_p.back().type_p = TpArrayDComplex;
	break;
    case TpString:
    case TpArrayString:
	data_p.back().type_p = TpArrayString;
	break;
    default:
        removeField (nfields()-1);
	throw (AipsError ("RecordDesc::addField(const String& fieldName, "
			  "DataType type) - unknown datatype (must be array"
			  "or ordinary scalar type)"));
    }
}

uInt RecordDescRep::addRecord (const String& fieldName, 
			       const RecordDesc& subDesc)
{
    addFieldName (fieldName, TpRecord);
    data_p.back().sub_record_p.reset (new RecordDesc(subDesc));
    return nfields();
}

uInt RecordDescRep::addTable (const String& fieldName, 
			      const String& tableDescName)
{
    addFieldName (fieldName, TpTable);
    data_p.back().tableDescName_p = tableDescName;
    return nfields();
}


const String& RecordDescRep::comment (Int whichField) const
{
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    return data_p[whichField].comment_p;
}

void RecordDescRep::setComment (Int whichField, const String& comment)
{
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    data_p[whichField].comment_p = comment;
}

void RecordDescRep::setShape (Int whichField, const IPosition& shape)
{
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    AlwaysAssert (isArray(whichField), AipsError);
    data_p[whichField].setArrayShape (shape);
}


uInt RecordDescRep::mergeField (const RecordDescRep& other,
				Int whichField,
				int duplicateAction)
{
    AlwaysAssert (whichField>=0 && whichField < Int(other.nfields()), AipsError);
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
    data_p.back().comment_p = other.comment (whichField);
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
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    // Remove the field from the name map.
    // Decrement the field number of all fields following it.
    name_map_p.erase (data_p[whichField].name_p);
    for (auto& x : name_map_p) {
        if (x.second > whichField) {
            x.second -= 1;
        }
    }
    data_p.erase (data_p.begin() + whichField);
    return nfields();
}

void RecordDescRep::renameField (const String& newName, Int whichField)
{
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    String oldName = data_p[whichField].name_p;
    Int inx = name_map_p[oldName];
    name_map_p.erase (oldName);
    name_map_p.insert (std::make_pair(newName, inx));
    data_p[whichField].name_p = newName;
}

void RecordDescRep::setShape (const IPosition& shape, Int whichField)
{
    AlwaysAssert (whichField>=0 && whichField < Int(nfields()), AipsError);
    data_p[whichField].shape_p = shape;
}

Int RecordDescRep::fieldNumber (const String& fieldName) const
{
    std::map<String,Int>::const_iterator iter = name_map_p.find (fieldName);
    return (iter == name_map_p.end()  ?  -1 : iter->second);
}

String RecordDescRep::makeName (Int whichField) const
{
    char strc[13];
    sprintf(strc, "*%i", whichField+1);
    return uniqueName (strc);
}

String RecordDescRep::uniqueName (const String& name) const
{
    String newName = name;
    char strc[16];      // should be plenty large...
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
    return *(data_p[whichField].sub_record_p);
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
    for (uInt i=0; i<n; ++i) {
	if (data_p[i].type_p != other.data_p[i].type_p) {
	    return False;
	}
	if (! data_p[i].shape_p.isEqual (other.data_p[i].shape_p)) {
	    return False;
	}
	if (data_p[i].tableDescName_p != other.data_p[i].tableDescName_p) {
	    return False;
	}
	if (static_cast<bool>(data_p[i].sub_record_p) != 
                              static_cast<bool>(other.data_p[i].sub_record_p)) {
            return False;
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
	if (data_p[i].sub_record_p) {
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
	Int whichField = other.fieldNumber (data_p[i].name_p);
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
	if (other.fieldNumber (data_p[i].name_p) >= 0) {
	    return False;                     // name exists in other
	}
    }
    return True;
}


} //# NAMESPACE CASACORE - END

