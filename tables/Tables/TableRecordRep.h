//# TableRecordRep.h: The representation of a TableRecord
//# Copyright (C) 1996,1997,2000,2001
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


#ifndef TABLES_TABLERECORDREP_H
#define TABLES_TABLERECORDREP_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordRep.h>
#include <casacore/tables/Tables/TableRecordData.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;
class TableAttr;


// <summary>
// The representation of a TableRecord
// </summary>

// <use visibility=local>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tTableRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="TableRecord">TableRecord</linkto>.
//   <li> <linkto class="RecordRep">RecordRep</linkto>.
// </prerequisite>
//
// <etymology>
// TableRecordRep is the REPresentation of a TableRecord.
// </etymology>
//
// <synopsis>
// TableRecordRep is the actual implementation of a TableRecord object.
// It contains the description and the data. The data is stored as
// a collection of void* pointers to the actual data. By storing
// it in this indirect way, it is easier to extend the data block.
// It also means that RecordFieldPtr objects always have the correct
// pointer and do not need to be adjusted when the data block is extended.
// <p>
// Despite the fact that the data pointers have type void*, the
// functions are completely type safe. This is done by passing the
// type around using the DataType enumeration. The downpart is that
// only types from that enumeration are supported (but that is also
// required by the RecordDesc mechanics).
// <p>
// Note that TableRecordRep does not know anything about RecordFieldPtr
// objects pointing to its data. Only its mother class TableRecord
// knows about them and handles all cases where the RecordFieldPtr's
// have to be notified.
// <p>
// Fields containing tables are not directly handled using class Table.
// Instead the class <linkto class=TableKeyword>TableKeyword</linkto>
// is used to map a table name to a table and to take care of
// opening a table on demand.
// </synopsis>
//
// <example>
// TableRecordRep mirrors all functions in TableRecord.
// </example>
//
// <motivation>
// Having a separate TableRecordRep class makes copy-on-write possible.
// It also allows derivation from RecordRep.
// </motivation>
//
//# <todo asof="1995/08/22">
//# </todo>

class TableRecordRep : public RecordRep
{
public:
    // Create a record with no fields.
    TableRecordRep() = default;

    // Create a record with the given description. If it is not possible to 
    // create all fields (for example, if a field of an unsupported type is
    // requested), an exception is thrown.
    // All fields are checked by the field checking function (if defined).
    TableRecordRep (const RecordDesc& description);

    // Create a copy of other using copy semantics.
    TableRecordRep (const TableRecordRep& other);

    // Copy all the data over.
    TableRecordRep& operator= (const TableRecordRep& other);
    
    // Delete all data.
    ~TableRecordRep() override;

    // Change the structure of this Record to contain the fields in
    // newDescription. After calling restructure, <src>description() ==
    // newDescription</src>.
    void restructure (const RecordDesc& newDescription, Bool recursive);

    // Returns True if this and other have the same RecordDesc, other
    // than different names for the fields. That is, the number, type and the
    // order of the fields must be identical (recursively for fixed
    // structured sub-Records in this).
    // <note role=caution>
    // <src>thisRecord.conform(thatRecord) == True</src> does not imply
    // <br><src>thatRecord.conform(thisRecord) == True</src>, because
    // a variable record in one conforms a fixed record in that, but
    // not vice-versa.
    // </note>
    Bool conform (const TableRecordRep& other) const;
  
    // Add a field with the given name and value to the record.
    // The data type of the field is determined by the data type of the value.
    // <group>
    void addField (const String& name, const TableRecord& value,
		   RecordInterface::RecordType type);
    void addField (const String& name, const Table& value,
		   RecordInterface::RecordType type);
    // </group>

    // Define a value for the given field.
    // Array conformance rules will not be applied for variable shaped arrays.
    // When the field and value data type mismatch, type promotion
    // of scalars will be done if possible. If not possible, an exception
    // is thrown.
  ////void defineDataField (Int whichField, DataType type, const void* value);

    // Close the table in the given field.
    // When accessed again, it will be opened automatically.
    // This can be useful to save memory usage.
    void closeTable (Int whichField) const;

    // Close all open tables.
    // When accessed again, it will be opened automatically.
    // This can be useful to save memory usage.
    void closeTables() const;

    // Flush all open subtables.
    void flushTables (Bool fsync) const;

    // Rename the subtables with a path containing the old parent table name.
    void renameTables (const String& newParentName,
		       const String& oldParentName);

    // Are subtables used in other processes.
    Bool areTablesMultiUsed() const;

    // Put the description and data of the Record.
    // It also puts the fixedFlag attribute (of the mother Record).
    void putRecord (AipsIO& os, Int recordType,
                    const TableAttr& parentAttr) const;

    // Get the description and data of the Record.
    // It also gets the fixedFlag attribute (of the mother Record).
    void getRecord (AipsIO& os, Int& recordType,
                    const TableAttr& parentAttr);

    // Put the data of a record.
    // This is used to write a subrecord, whose description has
    // already been written.
    void putData (AipsIO& os, const TableAttr&) const;

    // Read the data of a record.
    // This is used to read the name of a table or to read a record, whose
    // description has already been read.
    void getData (AipsIO& os, uInt version, const TableAttr&);

    // Reopen possible tables in keywords as read/write.
    // Tables are not reopened if they are not writable.
    void reopenRW();

    // Used by the RecordFieldPtr classes to attach in a type-safe way to the
    // correct field.
    // <group>
    void* get_pointer (Int whichField, DataType type) const override;
    void* get_pointer (Int whichField, DataType type,
		       const String& recordType) const override;
    // </group>

    // Do the actual merging of a field.
    void doMergeField (DataType type, const void* otherPtr,
                       const IPosition& shape) override;

    // Merge all fields from the other record into this record.
    void merge (const TableRecordRep& other,
		RecordInterface::DuplicatesFlag);

    void getTableKeySet (AipsIO& os, uInt version,
                         const TableAttr& parentAttr,
                         uInt type);
};


} //# NAMESPACE CASACORE - END

#endif
