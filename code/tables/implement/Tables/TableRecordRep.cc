//# TableRecordRep.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997
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

#include <aips/Tables/TableRecordRep.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableKeyword.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Lattices/IPosition.h>
#include <aips/IO/AipsIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


TableRecordRep::TableRecordRep ()
: RecordRep()
{}
	
TableRecordRep::TableRecordRep (const RecordDesc& description)
: RecordRep(),
  desc_p   (description)
{
    restructure (desc_p);
}

TableRecordRep::TableRecordRep (const TableRecordRep& other)
: RecordRep(),
  desc_p   (other.desc_p)
{
    restructure (desc_p);
    copy_other (other);
}

TableRecordRep& TableRecordRep::operator= (const TableRecordRep& other)
{
    if (this != &other) {
	restructure (other.desc_p);
	copy_other (other);
    }
    return *this;
}

TableRecordRep::~TableRecordRep()
{
    delete_myself (desc_p.nfields());
}

void TableRecordRep::restructure (const RecordDesc& newDescription)
{
    delete_myself (desc_p.nfields());
    desc_p  = newDescription;
    nused_p = desc_p.nfields();
    data_p.resize (nused_p);
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    data_p[i] = new 
TableRecord (this, desc_p.subRecord(i));
	} else if (desc_p.type(i) == TpTable) {
	    data_p[i] = new TableKeyword (desc_p.tableDescName(i));
	}else{
	    data_p[i] = createDataField (desc_p.type(i), desc_p.shape(i));
	}
	AlwaysAssert (data_p[i], AipsError);
    }
}

Int TableRecordRep::fieldNumber (const String& name) const
{
    return desc_p.fieldNumber (name);
}

void TableRecordRep::removeData (Int whichField, void* ptr)
{
    DataType type = desc_p.type(whichField);
    if (type == TpRecord) {
	delete (TableRecord*)ptr;
    } else if (type == TpTable) {
	delete (TableKeyword*)ptr;
    }else{
	deleteDataField (type, ptr);
    }
}

void TableRecordRep::addFieldToDesc (const String& name, DataType type,
				     const IPosition& shape, Bool fixedShape)
{
    if (fixedShape) {
	desc_p.addField (name, type, shape);
    }else{
	desc_p.addField (name, type);
    }
}

void TableRecordRep::removeFieldFromDesc (Int whichField)
{
    desc_p.removeField (whichField);
}

void TableRecordRep::addField (const String& name, const TableRecord& value,
			       RecordInterface::RecordType type)
{
    // When the record is empty, it is variable structured.
    if (value.nfields() == 0) {
	type = RecordInterface::Variable;
    }
    // When the new field is fixed, add its description too.
    if (type == RecordInterface::Fixed) {
	desc_p.addField (name, value.description());
    }else{
	desc_p.addField (name, TpRecord);
    }
    // Use default ctor and assignment to be sure that the
    // new record is variable structured.
    TableRecord* ptr = new TableRecord (this, type);
    *ptr = value;
    addDataPtr (ptr);
}

void TableRecordRep::addField (const String& name, const Table& value,
			       RecordInterface::RecordType type)
{
    String tableDescName;
    // When the new field is fixed, add its description name too.
    if (type == RecordInterface::Fixed) {
	tableDescName = value.tableDesc().getType();
    }
    desc_p.addTable (name, tableDescName);
    addDataPtr (new TableKeyword(value, tableDescName));
}

void TableRecordRep::defineDataField (Int whichField, DataType type,
				      const void* value)
{
    AlwaysAssert (whichField >= 0  &&  whichField < Int(nused_p)
		  &&  desc_p.type(whichField) == type, AipsError);
    if (type == TpRecord) {
	*(TableRecord*)data_p[whichField] = *(const TableRecord*)value;
    } else if (type == TpTable) {
	*(TableKeyword*)data_p[whichField] = *(const Table*)value;
    }else{
	if (desc_p.isArray(whichField)) {
	    const IPosition& shape = desc_p.shape(whichField);
	    if (shape.nelements() > 0  &&  shape(0) > 0) {
		checkShape (type, shape, value);
	    }
	}
	copyDataField (type, data_p[whichField], value);
    }
}

Bool TableRecordRep::conform (const TableRecordRep& other) const
{
    // First check (non-recursively) if the descriptions conform.
    if (! desc_p.conform (other.desc_p)) {
	return False;
    }
    // Now check for each fixed sub-record and table if it conforms.
    for (Int i=0; i<Int(nused_p); i++) {
	if (desc_p.type(i) == TpRecord) {
	    const TableRecord& thisRecord = *(const TableRecord*)data_p[i];
	    if (thisRecord.isFixed()) {
		const TableRecord& thatRecord =
		                      *(const TableRecord*)other.data_p[i];
		if (! thisRecord.conform (thatRecord)) {
		    return False;
		}
	    }
	} else if (desc_p.type(i) == TpTable) {
	    const TableKeyword& thisKey = *(const TableKeyword*)data_p[i];
	    if (thisKey.isFixed()) {
		const TableKeyword& thatKey =
		                      *(const TableKeyword*)other.data_p[i];
		if (! thisKey.conform (thatKey)) {
		    return False;
		}
	    }
	}
    }
    return True;
}

void TableRecordRep::copyData (const TableRecordRep& other)
{
    // Assume conform has already been called
    DebugAssert (conform (other), AipsError);
    copy_other (other);
}

void TableRecordRep::copy_other (const TableRecordRep& other)
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    *(TableRecord*)data_p[i] = *(const TableRecord*)other.data_p[i];
	} else if (desc_p.type(i) == TpTable) {
	    *(TableKeyword*)data_p[i] = *(const TableKeyword*)other.data_p[i];
	}else{
	    copyDataField (desc_p.type(i), data_p[i], other.data_p[i]);
	}
    }
}


void* TableRecordRep::get_pointer (Int whichField, DataType type,
				   const String& recordType) const
{
    AlwaysAssert (recordType == "TableRecord", AipsError);
    return get_pointer (whichField, type);
}
void* TableRecordRep::get_pointer (Int whichField, DataType type) const
{
    AlwaysAssert (whichField >= 0  &&  whichField < Int(desc_p.nfields())
		  &&  type == desc_p.type(whichField), AipsError);
    return data_p[whichField];
}

void TableRecordRep::closeTable (Int whichField) const
{
    AlwaysAssert (whichField >= 0  &&  whichField < Int(desc_p.nfields())
		  &&  desc_p.type(whichField) == TpTable, AipsError);
    ((TableKeyword*)data_p[whichField])->close();
}


void TableRecordRep::mergeField (const TableRecordRep& other,
				 Int whichFieldFromOther,
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
	    ptr = new TableRecord (*(TableRecord*)otherPtr);
	} else if (type == TpTable) {
	    ptr = new TableKeyword (*(TableKeyword*)otherPtr);
	}else{
	    ptr = createDataField (type, desc_p.shape(nr));
	    copyDataField (type, ptr, otherPtr);
	}
	addDataPtr (ptr);
    }
}

void TableRecordRep::merge (const TableRecordRep& other,
			    RecordInterface::DuplicatesFlag flag)
{
    Int n = other.desc_p.nfields();
    for (Int i=0; i<n; i++) {
	mergeField (other, i, flag);
    }
}
    

void TableRecordRep::renameTables (const String& newParentName,
				   const String& oldParentName)
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpTable) {
	    ((TableKeyword*)data_p[i])->renameTable (newParentName,
							   oldParentName);
	}
    }
}


void TableRecordRep::putRecord (AipsIO& os, int recordType,
				const String& parentTableName) const
{
    os.putstart ("TableRecord", 1);              // version 1
    os << desc_p;
    os << recordType;
    putData (os, parentTableName);
    os.putend();
}

void TableRecordRep::putData (AipsIO& os,
			      const String& parentTableName) const
{
    for (uInt i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		(*(const TableRecord*)data_p[i]).putRecord (os,
							    parentTableName);
	    }else{
		(*(const TableRecord*)data_p[i]).putData (os, parentTableName);
	    }
	} else if (desc_p.type(i) == TpTable) {
	    os << ((const TableKeyword*)data_p[i])->tableName(parentTableName);
	}else{
	    putDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void TableRecordRep::getRecord (AipsIO& os, int& recordType, Bool openWritable,
				const String& parentTableName)
{
    // Support reading scalar, array, and table keyword sets as records.
    uInt version;
    String type = os.getNextType();
    if (type == "ScalarKeywordSet") {
	version = os.getstart ("ScalarKeywordSet");
	getTableKeySet (os, version, openWritable, parentTableName, 0);
    } else if (type == "ArrayKeywordSet") {
	version = os.getstart ("ArrayKeywordSet");
	getTableKeySet (os, version, openWritable, parentTableName, 1);
    } else if (type == "TableKeywordSet") {
	version = os.getstart ("TableKeywordSet");
	getTableKeySet (os, version, openWritable, parentTableName, 2);
	recordType = RecordInterface::Variable;
    }else{
	uInt version = os.getstart ("TableRecord");
	// Get the description and restructure the record.
	RecordDesc desc;
	os >> desc;
	os >> recordType;
	restructure (desc);
	// Read the data.
	getData (os, version, openWritable, parentTableName);
    }
    os.getend();
}

void TableRecordRep::getData (AipsIO& os, uInt version, Bool openWritable,
			      const String& parentTableName)
{
    for (uInt i=0; i<nused_p; i++) {
	DataType type = desc_p.type(i);
	if (type == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		(*(TableRecord*)data_p[i]).getRecord (os, openWritable,
						      parentTableName);
	    }else{
		(*(TableRecord*)data_p[i]).getData (os, version, openWritable,
						    parentTableName);
	    }
	}else if (type == TpTable) {
	    String name;
	    os >> name;
	    ((TableKeyword*)data_p[i])->set (name, openWritable,
					     parentTableName);
	}else{
	    getDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void TableRecordRep::reopenRW()
{
    for (uInt i=0; i<nused_p; i++) {
	DataType type = desc_p.type(i);
	if (type == TpRecord) {
	    (*(TableRecord*)data_p[i]).reopenRW();
	}else if (type == TpTable) {
	    ((TableKeyword*)data_p[i])->setRW();
	}
    }
}

void TableRecordRep::getTableKeySet (AipsIO& os, uInt version,
				     Bool openWritable,
				     const String& parentTableName,
				     uInt type)
{
    // First build the description from the map of keyword names and
    // attributes.
    RecordDesc desc;
    getKeyDesc (os, desc);
    // Define the record from the description.
    // Read the keyword values and define the corresponding record value.
    restructure (desc);
    getScalarKeys (os);
    if (type > 0) {
	getArrayKeys (os);
    }
    if (type > 1) {
	String key, name;
	uInt i, n;
	os >> n;
	for (i=0; i<n; i++) {
	    os >> key;               // keyword name
	    os >> name;              // table name
	    ((TableKeyword*)data_p[desc_p.fieldNumber(key)])->set
	                               (name, openWritable, parentTableName);
	}
    }
    // Newer keyword sets may contain nested keyword sets.
    // We do not support reading those, so throw an exception when they exist.
    if (version > 1) {
	uInt n;
	os >> n;
	AlwaysAssert (n==0, AipsError);
    }
}
