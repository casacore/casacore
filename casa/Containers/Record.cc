//# Record.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1995,1996,1997,1998,1999,2001
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
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Record::Record()
: RecordInterface (),
  rep_p    (new RecordRep),
  parent_p (0)
{}

Record::Record (RecordType type,
		CheckFieldFunction* func, const void* checkArgument)
: RecordInterface (type, func, checkArgument),
  rep_p    (new RecordRep),
  parent_p (0)
{}
	
Record::Record (const RecordDesc& description, RecordType type,
		CheckFieldFunction* func, const void* checkArgument)
: RecordInterface (type, func, checkArgument),
  rep_p    (new RecordRep (description)),
  parent_p (0)
{}

// When description is empty, Record structure is variable.
Record::Record (RecordRep* parent, const RecordDesc& description)
: RecordInterface (description.nfields()==0 ? Variable : Fixed, 0, 0),
  rep_p    (new RecordRep (description)),
  parent_p (parent)
{}

Record::Record (RecordRep* parent, RecordType type)
: RecordInterface (type, 0, 0),
  rep_p    (new RecordRep),
  parent_p (parent)
{}

Record::Record (const Record& other)
: RecordInterface (other),
  rep_p    (other.rep_p),
  parent_p (other.parent_p)
{}

Record::Record (const RecordInterface& other)
: RecordInterface (other),
  rep_p    (new RecordRep (other.description())),
  parent_p (0)
{
    uInt n = other.nfields();
    const RecordDesc& desc = description();
    for (uInt i=0; i<n; i++) {
	DataType dtype = desc.type(i);
	if (dtype == TpRecord) {
	    const RecordInterface& subrec = *((RecordInterface*)
                                              (other.get_pointer (i, dtype)));
	    defineRecord (i, Record (subrec),
			  subrec.isFixed()  ?  Fixed : Variable);
	}else{
	    rep_p->copyDataField (dtype, i, other.get_pointer (i, dtype));
	}
    }
}

Record& Record::operator= (const Record& other)
{
    // Assignment is only possible when the Record is empty or
    // when their layout match or when the Record is non-fixed.
    // When non-fixed or empty, we simply replace the representation.
    // Otherwise we replace all values (in which case we do not need
    // to replace the pointers in RecordFieldPtr's).
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

Record::~Record()
{}

RecordInterface* Record::clone() const
{
    return new Record (*this);
}

void Record::assign (const RecordInterface& that)
{
    *this = that;
}

void Record::print (ostream& os, Int maxNrValues, const String& indent) const
{
    rep_p.ref().print (os, maxNrValues, indent);
}

void Record::makeUnique()
{
    rwRef();
}

RecordRep& Record::rwRef()
{
    const RecordRep& oldRep = rep_p.ref();
    RecordRep& newRep = rep_p.rwRef();
    if (&oldRep != &newRep) {
	notify (RecordNotice (RecordNotice::ACQUIRE, 0));
    }
    return newRep;
}

const String& Record::comment (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return ref().comment (whichField);
}
void Record::setComment (const RecordFieldId& id, const String& comment)
{
    Int whichField = idToNumber (id);
    rwRef().setComment (whichField, comment);
}

RecordDesc Record::getDescription() const
{
    return ref().description();
}

void Record::restructure (const RecordDesc& newDescription, Bool recursive)
{
    // Restructure is not possible for fixed records.
    throwIfFixed();
    // Restructuring means that all RecordFieldPtr's get invalid.
    notify (RecordNotice (RecordNotice::DETACH, 0));
    rwRef().restructure (newDescription, recursive);
}

uInt Record::nfields() const
{
    return description().nfields();
}
Int Record::fieldNumber (const String& fieldName) const
{
    return description().fieldNumber (fieldName);
}
DataType Record::type (Int whichField) const
{
    return description().type (whichField);
}

void Record::removeField (const RecordFieldId& id)
{
    throwIfFixed();
    Int whichField = idToNumber (id);
    rwRef().removeField (whichField);
    notify (RecordNotice (RecordNotice::REMOVE, whichField));
}

void Record::renameField (const String& newName, const RecordFieldId& id)
{
    rwRef().renameField (newName, idToNumber(id));
}


void Record::addDataField (const String& name, DataType type,
			   const IPosition& shape, Bool fixedShape,
			   const void* value)
{
    rwRef().addDataField (name, type, shape, fixedShape, value);
}

void Record::defineDataField (Int whichField, DataType type,
			      const void* value)
{
    rwRef().defineDataField (whichField, type, value);
}

void* Record::get_pointer (Int whichField, DataType type) const
{
    return ref().get_pointer (whichField, type);
}
void* Record::get_pointer (Int whichField, DataType type,
			   const String& recordType) const
{
    return ref().get_pointer (whichField, type, recordType);
}

void Record::defineRecord (const RecordFieldId& id,
			   const RecordInterface& value, RecordType type)
{
    defineRecord (id, Record (value), type);
}
void Record::defineRecord (const RecordFieldId& id,
			   const Record& value, RecordType type)
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
	Record& subrec = *(Record*)get_pointer (whichField, TpRecord);
	subrec.recordType() = type;
    }
}


const RecordInterface& Record::asRecord (const RecordFieldId& id) const
{
    return subRecord (id);
}
RecordInterface& Record::asrwRecord (const RecordFieldId& id)
{
    return rwSubRecord (id);
}
const Record& Record::subRecord (const RecordFieldId& id) const
{
    Int whichField = idToNumber (id);
    return *(const Record*)get_pointer (whichField, TpRecord);
}
Record& Record::rwSubRecord (const RecordFieldId& id)
{
    Int whichField = idToNumber (id);
    rwRef();
    return *(Record*)get_pointer (whichField, TpRecord);
}    


void Record::mergeField (const Record& other, const RecordFieldId& id,
			 DuplicatesFlag flag)
{
    throwIfFixed();
    Int whichField = other.idToNumber (id);
    rwRef().mergeField (other.ref(), whichField, flag);
}

void Record::merge (const Record& other, DuplicatesFlag flag)
{
    AlwaysAssert (this != &other, AipsError);
    throwIfFixed();
    rwRef().merge (other.ref(), flag);
}


void Record::putRecord (AipsIO& os) const
{
    ref().putRecord (os, recordType());
}
void Record::getRecord (AipsIO& os)
{
    // Get is only possible when the Record is empty or when
    // the Record is non-fixed.
    AlwaysAssert ((! isFixed()  ||  nfields() == 0), AipsError);
    // Possible RecordFieldPtr pointers have to be removed.
    notify (RecordNotice (RecordNotice::DETACH, 0));
    // Reading the record type back means casting it from an int
    // to the correct type.
    Int type;
    rwRef().getRecord (os, type);
    recordType() = (RecordInterface::RecordType)type;
}

} //# NAMESPACE CASACORE - END

