//# TableRecord.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1995,1996,1997,1998
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

#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableKeyword.h>
#include <aips/Tables/Table.h>
#include <aips/IO/AipsIO.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>


TableRecord::TableRecord()
: RecordInterface (),
  rep_p    (new TableRecordRep),
  parent_p (0)
{}

TableRecord::TableRecord (RecordType type,
			  CheckFieldFunction* func, const void* checkArgument)
: RecordInterface (type, func, checkArgument),
  rep_p    (new TableRecordRep),
  parent_p (0)
{}
	
TableRecord::TableRecord (const RecordDesc& description,
			  RecordType type,
			  CheckFieldFunction* func, const void* checkArgument)
: RecordInterface (type, func, checkArgument),
  rep_p    (new TableRecordRep (description)),
  parent_p (0)
{}

// When description is empty, TableRecord is not fixed.
TableRecord::TableRecord (TableRecordRep* parent,
			  const RecordDesc& description)
: RecordInterface (description.nfields()==0 ? Variable : Fixed, 0, 0),
  rep_p    (new TableRecordRep (description)),
  parent_p (parent)
{}

TableRecord::TableRecord (TableRecordRep* parent, RecordType type)
: RecordInterface (type, 0, 0),
  rep_p    (new TableRecordRep),
  parent_p (parent)
{}

TableRecord::TableRecord (const TableRecord& other)
: RecordInterface (other),
  rep_p    (other.rep_p),
  parent_p (other.parent_p)
{}

TableRecord::TableRecord (const RecordInterface& other)
: RecordInterface (other),
  rep_p    (new TableRecordRep (other.description())),
  parent_p (0)
{
    uInt n = other.nfields();
    const RecordDesc& desc = description();
    for (uInt i=0; i<n; i++) {
	DataType dtype = desc.type(i);
	if (dtype == TpRecord) {
	    const RecordInterface& subrec = *((RecordInterface*)
                                              (other.get_pointer (i, dtype)));
	    defineRecord (i, TableRecord (subrec),
			  subrec.isFixed()  ?  Fixed : Variable);
	}else{
	    rep_p->copyDataField (dtype, i, other.get_pointer (i, dtype));
	}
    }
}

TableRecord& TableRecord::operator= (const TableRecord& other)
{
    // Assignment is only possible when the Record is empty or
    // when their layout match or when the Record is non-fixed.
    // When non-fixed or empty, we simply replace the representation.
    // Otherwise we replace all values (in which case we do not need
    // to replace the RecordFieldPtr pointers).
    if (this != &other) {
	if (! isFixed()  ||  nfields() == 0) {
	    notify (RecordNotice (RecordNotice::DETACH, 0));
	    rep_p = other.rep_p;
	}else{
	    AlwaysAssert (conform (other), AipsError);
	    rwRef().copyData (other.ref());
	}
    }
    return *this;
}

TableRecord::~TableRecord()
{}

RecordInterface* TableRecord::clone() const
{
    return new TableRecord (*this);
}

void TableRecord::assign (const RecordInterface& that)
{
    TableRecord tmp (that);
    *this = that;
}


void TableRecord::makeUnique()
{
    rwRef();
}

TableRecordRep& TableRecord::rwRef()
{
    const TableRecordRep& oldRep = rep_p.ref();
    TableRecordRep& newRep = rep_p.rwRef();
    if (&oldRep != &newRep) {
	notify (RecordNotice (RecordNotice::ACQUIRE, 0));
    }
    return newRep;
}

const String& TableRecord::comment (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return ref().comment (whichField);
}
void TableRecord::setComment (const RecordFieldId& id, const String& comment)
{
    Int whichField = idToNumber (id);
    rwRef().setComment (whichField, comment);
}

RecordDesc TableRecord::getDescription() const
{
    return ref().description();
}

void TableRecord::restructure (const RecordDesc& newDescription)
{
    // Restructure is not possible for fixed records.
    throwIfFixed();
    // Restructuring means that all RecordFieldPtr's get invalid.
    notify (RecordNotice (RecordNotice::DETACH, 0));
    rwRef().restructure (newDescription);
}

void TableRecord::restructure (const RecordDesc& newDescription,
			       RecordType type)
{
    restructure (newDescription);
    setRecordType (type);
}

void TableRecord::setRecordType (RecordType rtype)
{
    recordType() = rtype;
    // Iterate through all fields to make the subrecords the required type.
    uInt nf = nfields();
    for (uInt i=0; i<nf; i++) {
	if (type(i) == TpRecord) {
	    rwSubRecord(i).setRecordType (rtype);
	}
    }
}

uInt TableRecord::nfields() const
{
    return description().nfields();
}
Int TableRecord::fieldNumber (const String& fieldName) const
{
    return description().fieldNumber (fieldName);
}
DataType TableRecord::type (Int whichField) const
{
    return description().type (whichField);
}

void TableRecord::removeField (const RecordFieldId& id)
{
    throwIfFixed();
    Int whichField = idToNumber (id);
    rwRef().removeField (whichField);
    notify (RecordNotice (RecordNotice::REMOVE, whichField));
}

void TableRecord::renameField (const String& newName, const RecordFieldId& id)
{
    rwRef().renameField (newName, idToNumber(id));
}

void TableRecord::addDataField (const String& name, DataType type,
				const IPosition& shape, Bool fixedShape,
				const void* value)
{
    rwRef().addDataField (name, type, shape, fixedShape, value);
}

void TableRecord::defineDataField (Int whichField, DataType type,
				   const void* value)
{
    rwRef().defineDataField (whichField, type, value);
}

void* TableRecord::get_pointer (Int whichField, DataType type) const
{
    return ref().get_pointer (whichField, type);
}
void* TableRecord::get_pointer (Int whichField, DataType type,
				const String& recordType) const
{
    return ref().get_pointer (whichField, type, recordType);
}

void TableRecord::defineRecord (const RecordFieldId& id,
				const RecordInterface& value, RecordType type)
{
    defineRecord (id, TableRecord (value), type);
}
void TableRecord::defineRecord (const RecordFieldId& id,
				const TableRecord& value,
				RecordInterface::RecordType type)
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
	checkName (name, TpRecord);
	rwRef().addField (name, value, type);
    }else{
	rwRef().defineDataField (whichField, TpRecord, &value);
	TableRecord& subrec = *(TableRecord*)get_pointer (whichField, TpRecord);
	subrec.recordType() = type;
    }
}

void TableRecord::defineTable (const RecordFieldId& id,
			       const Table& value,
			       RecordInterface::RecordType type)
{
    Int whichField = newIdToNumber (id);
    if (whichField < 0  &&  id.byName()) {
	throwIfFixed();
	checkName (id.fieldName(), TpTable);
	rwRef().addField (id.fieldName(), value, type);
    }else{
	rwRef().defineDataField (whichField, TpTable, &value);
    }
}

const RecordInterface& TableRecord::asRecord (const RecordFieldId& id) const
{
    return subRecord (id);
}
RecordInterface& TableRecord::asrwRecord (const RecordFieldId& id)
{
    return rwSubRecord (id);
}
const TableRecord& TableRecord::subRecord (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const TableRecord*)get_pointer (whichField, TpRecord);
}
TableRecord& TableRecord::rwSubRecord (const RecordFieldId& id)
{
    Int whichField = idToNumber (id);
    rwRef();
    return *(TableRecord*)get_pointer (whichField, TpRecord);
}    

Table TableRecord::asTable (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return ((const TableKeyword*)get_pointer (whichField, TpTable))->table();
}

void TableRecord::closeTable (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    ref().closeTable (whichField);
}


void TableRecord::mergeField (const TableRecord& other,
			      const RecordFieldId& id, DuplicatesFlag flag)
{
    throwIfFixed();
    Int whichField = other.idToNumber (id);
    rwRef().mergeField (other.ref(), whichField, flag);
}

void TableRecord::merge (const TableRecord& other, DuplicatesFlag flag)
{
    AlwaysAssert (this != &other, AipsError);
    throwIfFixed();
    rwRef().merge (other.ref(), flag);
}


void TableRecord::putRecord (AipsIO& os,
			     const String& parentTableName) const
{
    ref().putRecord (os, recordType(), parentTableName);
}
void TableRecord::getRecord (AipsIO& os, Bool openWritable,
			     const String& parentTableName)
{
    // Get is only possible when the Record is empty or when
    // the Record is non-fixed.
    AlwaysAssert ((! isFixed()  ||  nfields() == 0), AipsError);
    // Possible RecordFieldPtr's have to be detached.
    notify (RecordNotice (RecordNotice::DETACH, 0));
    // Reading the record type back means casting it from an int
    // to the correct type.
    int type;
    rwRef().getRecord (os, type, openWritable, parentTableName);
    recordType() = (RecordInterface::RecordType)type;
}
