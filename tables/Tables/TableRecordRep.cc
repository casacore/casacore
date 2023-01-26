//# TableRecordRep.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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

#include <casacore/tables/Tables/TableRecordRep.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableKeyword.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableRecordRep::TableRecordRep ()
: RecordRep()
{}
	
TableRecordRep::TableRecordRep (const RecordDesc& description)
: RecordRep(),
  desc_p   (description)
{
    restructure (desc_p, true);
}

TableRecordRep::TableRecordRep (const TableRecordRep& other)
: RecordRep(),
  desc_p   (other.desc_p)
{
    restructure (desc_p, false);
    copy_other (other);
}

TableRecordRep& TableRecordRep::operator= (const TableRecordRep& other)
{
    if (this != &other) {
	restructure (other.desc_p, false);
	copy_other (other);
    }
    return *this;
}

TableRecordRep::~TableRecordRep()
{
    delete_myself (desc_p.nfields());
}

void TableRecordRep::restructure (const RecordDesc& newDescription,
				  bool recursive)
{
    delete_myself (desc_p.nfields());
    desc_p  = newDescription;
    nused_p = desc_p.nfields();
    datavec_p.resize (nused_p);
    datavec_p = static_cast<void*>(0);
    data_p.resize (nused_p);
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    if (recursive) {
	        data_p[i] = new TableRecord (this, desc_p.subRecord(i));
	    } else {
	        data_p[i] = new TableRecord (this, RecordDesc());
	    }
	} else if (desc_p.type(i) == TpTable) {
	    data_p[i] = new TableKeyword (desc_p.tableDescName(i));
	}else{
	    data_p[i] = createDataField (desc_p.type(i), desc_p.shape(i));
	}
	AlwaysAssert (data_p[i], AipsError);
    }
}

int32_t TableRecordRep::fieldNumber (const String& name) const
{
    return desc_p.fieldNumber (name);
}

void TableRecordRep::removeData (int32_t whichField, void* ptr, void* vecptr)
{
    DataType type = desc_p.type(whichField);
    if (type == TpRecord) {
	delete static_cast<TableRecord*>(ptr);
    } else if (type == TpTable) {
	delete static_cast<TableKeyword*>(ptr);
    }else{
	deleteDataField (type, ptr, vecptr);
    }
}

void TableRecordRep::addFieldToDesc (const String& name, DataType type,
				     const IPosition& shape, bool fixedShape)
{
    if (fixedShape) {
	desc_p.addField (name, type, shape);
    }else{
	desc_p.addField (name, type);
    }
}

void TableRecordRep::removeFieldFromDesc (int32_t whichField)
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

void TableRecordRep::defineDataField (int32_t whichField, DataType type,
				      const void* value)
{
    AlwaysAssert (whichField >= 0  &&  whichField < int32_t(nused_p), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
        if (type == TpRecord) {
	    *static_cast<TableRecord*>(data_p[whichField]) =
	      *static_cast<const TableRecord*>(value);
	} else if (type == TpTable) {
	    *static_cast<TableKeyword*>(data_p[whichField]) =
	      *static_cast<const Table*>(value);
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
        throw (AipsError ("TableRecordRep::defineDataField - "
			  "incorrect data type used for field " +
			  desc_p.name(whichField)));
    }
}

bool TableRecordRep::conform (const TableRecordRep& other) const
{
    // First check (non-recursively) if the descriptions conform.
    if (! desc_p.conform (other.desc_p)) {
	return false;
    }
    // Now check for each fixed sub-record and table if it conforms.
    for (int32_t i=0; i<int32_t(nused_p); i++) {
	if (desc_p.type(i) == TpRecord) {
	    const TableRecord& thisRecord =
	      *static_cast<TableRecord*>(const_cast<void*>(data_p[i]));
	    if (thisRecord.isFixed()) {
		const TableRecord& thatRecord =
		  *static_cast<TableRecord*>(const_cast<void*>(other.data_p[i]));
		if (! thisRecord.conform (thatRecord)) {
		    return false;
		}
	    }
	} else if (desc_p.type(i) == TpTable) {
	    const TableKeyword& thisKey =
	      *static_cast<TableKeyword*>(const_cast<void*>(data_p[i]));
	    if (thisKey.isFixed()) {
		const TableKeyword& thatKey =
		  *static_cast<TableKeyword*>(const_cast<void*>(other.data_p[i]));
		if (! thisKey.conform (thatKey)) {
		    return false;
		}
	    }
	}
    }
    return true;
}

void TableRecordRep::copyData (const TableRecordRep& other)
{
    // Assume conform has already been called
    DebugAssert (conform (other), AipsError);
    copy_other (other);
}

void TableRecordRep::copy_other (const TableRecordRep& other)
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    *static_cast<TableRecord*>(data_p[i]) =
	      *static_cast<TableRecord*>(const_cast<void*>(other.data_p[i]));
	} else if (desc_p.type(i) == TpTable) {
	    *static_cast<TableKeyword*>(data_p[i]) =
	      *static_cast<TableKeyword*>(const_cast<void*>(other.data_p[i]));
	}else{
	    copyDataField (desc_p.type(i), data_p[i], other.data_p[i]);
	}
    }
}


void* TableRecordRep::get_pointer (int32_t whichField, DataType type,
				   const String& recordType) const
{
    if (recordType != "TableRecord") {
        throw (AipsError ("TableRecordRep::get_pointer - field " +
			  desc_p.name(whichField) +
			  " is not of type TableRecord"));
    }
    return get_pointer (whichField, type);
}
void* TableRecordRep::get_pointer (int32_t whichField, DataType type) const
{
    AlwaysAssert (whichField >= 0  &&  whichField < int32_t(nused_p), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
        return data_p[whichField];
    }
    // A scalar can be returned as an array.
    if (! (isArray(type)  &&  asScalar(type) == descDtype)) {
        throw (AipsError ("TableRecordRep::get_pointer - "
			  "incorrect data type used for field " +
			  desc_p.name(whichField)));
    }
    if (datavec_p[whichField] == nullptr) {
        const_cast<TableRecordRep*>(this)->makeDataVec (whichField, descDtype);
    }
    return datavec_p[whichField];
}

void TableRecordRep::closeTable (int32_t whichField) const
{
    AlwaysAssert (whichField >= 0  &&  whichField < int32_t(desc_p.nfields())
		  &&  desc_p.type(whichField) == TpTable, AipsError);
    static_cast<TableKeyword*>(const_cast<void*>(data_p[whichField]))->close();
}


void TableRecordRep::mergeField (const TableRecordRep& other,
				 int32_t whichFieldFromOther,
				 RecordInterface::DuplicatesFlag flag)
{
    // If the field exists and if flag tells to overwrite,
    // the field is removed first.
    if (flag == RecordInterface::OverwriteDuplicates) {
	int32_t fld = desc_p.fieldNumber (other.desc_p.name(whichFieldFromOther));
	if (fld >= 0) {
	    removeField (fld);
	}
    }
    // Try to add the field to the description.
    int32_t nr = desc_p.nfields();
    int32_t nrnew = desc_p.mergeField (other.desc_p, whichFieldFromOther, flag);
    // It succeeded if nfields increased.
    // Then the value can be defined.
    if (nrnew > nr) {
	DataType type = desc_p.type (nr);
	void* otherPtr = other.get_pointer (whichFieldFromOther, type);
	void* ptr;
	if (type == TpRecord) {
	    ptr = new TableRecord (*static_cast<TableRecord*>(otherPtr));
	} else if (type == TpTable) {
	    ptr = new TableKeyword (*static_cast<TableKeyword*>(otherPtr));
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
    int32_t n = other.desc_p.nfields();
    for (int32_t i=0; i<n; i++) {
	mergeField (other, i, flag);
    }
}
    

void TableRecordRep::renameTables (const String& newParentName,
				   const String& oldParentName)
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpTable) {
	    static_cast<TableKeyword*>(data_p[i])->renameTable (newParentName,
								oldParentName);
	}
    }
}


void TableRecordRep::closeTables() const
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpTable) {
	    static_cast<TableKeyword*>(const_cast<void*>(data_p[i]))->close();
	}
    }
}


void TableRecordRep::flushTables (bool fsync) const
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpTable) {
	    static_cast<TableKeyword*>(const_cast<void*>(data_p[i]))->flush(fsync);
	}
    }
}


bool TableRecordRep::areTablesMultiUsed() const
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpTable) {
	    if (static_cast<TableKeyword*>(const_cast<void*>(data_p[i]))->isMultiUsed(true)) {
	        return true;
	    }
	}
    }
    return false;
}


void TableRecordRep::print (std::ostream& os, int32_t maxNrValues,
			    const String& indent) const
{
    for (uint32_t i=0; i<nused_p; i++) {
        os << indent << desc_p.name(i) << ": ";
	if (desc_p.type(i) == TpRecord) {
	    os << '{' << endl;
	    static_cast<const TableRecord*>(data_p[i])->print(os, maxNrValues,
							      indent+"  ");
	    os << indent << '}' << endl;
	} else if (desc_p.type(i) == TpTable) {
	    os << "Table "
	       << static_cast<const TableKeyword*>(data_p[i])->tableName()
               << endl;
        } else {
	    printDataField (os, desc_p.type(i),
			    indent, maxNrValues, data_p[i]);
	    os << endl;
	}
    }
}

void TableRecordRep::putRecord (AipsIO& os, int32_t recordType,
				const TableAttr& parentAttr) const
{
    os.putstart ("TableRecord", 1);              // version 1
    os << desc_p;
    os << recordType;
    putData (os, parentAttr);
    os.putend();
}

void TableRecordRep::putData (AipsIO& os, const TableAttr& parentAttr) const
{
    for (uint32_t i=0; i<nused_p; i++) {
	if (desc_p.type(i) == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		static_cast<TableRecord*>(const_cast<void*>(data_p[i]))->putRecord (os,
								 parentAttr);
	    }else{
		static_cast<TableRecord*>(const_cast<void*>(data_p[i]))->putData (os, parentAttr);
	    }
	} else if (desc_p.type(i) == TpTable) {
	  os << static_cast<TableKeyword*>(const_cast<void*>(data_p[i]))->tableName (parentAttr);
	}else{
	    putDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void TableRecordRep::getRecord (AipsIO& os, int32_t& recordType,
				const TableAttr& parentAttr)
{
    // Support reading scalar, array, and table keyword sets as records.
    uint32_t version;
    String type = os.getNextType();
    if (type == "ScalarKeywordSet") {
	version = os.getstart ("ScalarKeywordSet");
	getTableKeySet (os, version, parentAttr, 0);
    } else if (type == "ArrayKeywordSet") {
	version = os.getstart ("ArrayKeywordSet");
	getTableKeySet (os, version, parentAttr, 1);
    } else if (type == "TableKeywordSet") {
	version = os.getstart ("TableKeywordSet");
	getTableKeySet (os, version, parentAttr, 2);
	recordType = RecordInterface::Variable;
    }else{
	uint32_t version = os.getstart ("TableRecord");
	// Get the description and restructure the record.
	RecordDesc desc;
	os >> desc;
	os >> recordType;
	restructure (desc, true);
	// Read the data.
	getData (os, version, parentAttr);
    }
    os.getend();
}

void TableRecordRep::getData (AipsIO& os, uint32_t version,
			      const TableAttr& parentAttr)
{
    for (uint32_t i=0; i<nused_p; i++) {
	DataType type = desc_p.type(i);
	if (type == TpRecord) {
	    const RecordDesc& desc = desc_p.subRecord(i);
	    if (desc.nfields() == 0) {
		static_cast<TableRecord*>(data_p[i])->getRecord
		                                   (os, parentAttr);
	    }else{
		static_cast<TableRecord*>(data_p[i])->getData
		                                   (os, version, parentAttr);
	    }
	}else if (type == TpTable) {
	    String name;
	    os >> name;
	    static_cast<TableKeyword*>(data_p[i])->set (name, parentAttr);
	}else{
	    getDataField (os, desc_p.type(i), data_p[i]);
	}
    }
}

void TableRecordRep::reopenRW()
{
    for (uint32_t i=0; i<nused_p; i++) {
	DataType type = desc_p.type(i);
	if (type == TpRecord) {
	    static_cast<TableRecord*>(data_p[i])->reopenRW();
	}else if (type == TpTable) {
	    static_cast<TableKeyword*>(data_p[i])->setRW();
	}
    }
}

void TableRecordRep::getTableKeySet (AipsIO& os, uint32_t version,
				     const TableAttr& parentAttr,
				     uint32_t type)
{
    // First build the description from the map of keyword names and
    // attributes.
    RecordDesc desc;
    getKeyDesc (os, desc);
    // Define the record from the description.
    // Read the keyword values and define the corresponding record value.
    restructure (desc, true);
    getScalarKeys (os);
    if (type > 0) {
	getArrayKeys (os);
    }
    if (type > 1) {
	String key, name;
	uint32_t i, n;
	os >> n;
	for (i=0; i<n; i++) {
	    os >> key;               // keyword name
	    os >> name;              // table name
	    static_cast<TableKeyword*>(data_p[desc_p.fieldNumber(key)])->set
	                                           (name, parentAttr);
	}
    }
    // Newer keyword sets may contain nested keyword sets.
    // We do not support reading those, so throw an exception when they exist.
    if (version > 1) {
	uint32_t n;
	os >> n;
	AlwaysAssert (n==0, AipsError);
    }
}

} //# NAMESPACE CASACORE - END

