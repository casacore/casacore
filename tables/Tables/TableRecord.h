//# TableRecord.h: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997,1998,2000,2001,2002
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


#ifndef TABLES_TABLERECORD_H
#define TABLES_TABLERECORD_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/tables/Tables/TableRecordRep.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Utilities/COWPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Array;
class IPosition;
class AipsIO;
class TableLock;


// <summary>
// A hierarchical collection of named fields of various types
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tTableRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordDesc">RecordDesc</linkto>.
//   <li> <linkto class="RecordFieldPtr">RecordFieldPtr</linkto>.
// </prerequisite>
//
// <etymology>
// TableRecord is a Record to be used in the Table system.
// </etymology>
//
// <synopsis>
// Class <linkto class=RecordInterface>RecordInterface</linkto> describes
// the fundamental properties of records.
// <br>
// The TableRecord class is a particular type of a record class.
// The fields in TableRecord may be of scalar type, array type, a Table
// or a TableRecord.
// The types are chosen to be compatible with the native
// types of the Table system, viz: Bool, uChar, Short, Int, uInt, Float,
// Double, Complex, DComplex, String.
// Arrays of all these types are also available.
// Note that a TableRecord is not a space-efficient way of storing
// small objects.
// <p>
// The structure of a TableRecord is defined by the
// <linkto class="RecordDesc">RecordDesc</linkto> class.
// The structure of the TableRecord can be defined at
// construction time. It can thereafter be restructured. This has the
// effect, however, that any existing RecordFieldPtr objects become
// invalid (using the <linkto file="Notice.h">Notice</linkto> classes).
// <br>
// It is possible to add or remove fields once a TableRecord is constructed.
// However, this is not possible when the TableRecord is constructed with a
// fixed structure (i.e. with the fixedStructure flag set).
// <p>
// A TableRecord is an hierarchical structure, because it can have fields
// containing TableRecord's (as layed out in the RecordDesc). A subrecord
// has a variable structure, when its RecordDesc is empty (i.e. contains
// no fields). It is fixed when its RecordDesc contains fields.
// <p>
// A TableRecord may be assigned to another only if they conform; that is if
// their fields have the identical type in the identical order.
// The field names do not need to be identical however, only the types.
// That is, the structure needs to be identical, but
// not the labels. Note that field order is significant, 
// <src>[ifield(type=Int),ffield(type=Float)]</src>
// is not the same as <src>[ffield(type=Float),ifield(type=Int)]</src>
// <br>
// Conformance is checked recursively for fixed subrecords. That is, a
// variable structured subrecord is not checked, because any record
// can be assigned to it. A fixed structured subrecord has to
// conform the corresponding subrecord in the source.
// <br> A Table field is conforming when the name of the table
// description of the source table matches the table description name
// defined in the RecordDesc field. When that name is blank, every
// table matches. In fact, defining a table description name is identical
// to defining an array shape..
// <p>
// When a TableRecord is read back, possible Tables contained in fields
// are only opended and read back when they are accessed for the first time.
// In that way no needless table opens are done.
// When a table has been opened, it is possible to close it. This
// can be useful to save memory usage.
// <p>
// TableRecord uses copy-on-write semantics. This means that when a
// TableRecord is copied, only the pointer to the underlying
// TableRecordRep object is copied.
// Only when the TableRecord gets changed (i.e. when a non-const
// TableRecord member function is called), the TableRecordRep object is copied.
// This results in a cheap copy behaviour.
// </synopsis>
//
// <example>
// <srcblock>
//  {
//    TableDesc td ("td", TableDesc::Scratch);
//    td.addColumn (ScalarColumnDesc<Int> ("col1"));
//    td.addColumn (ScalarColumnDesc<float> ("col2"));
//    SetupNewTable newtab ("tTableRecord_tmp.tab1", td1, Table::New);
//    Table tab (newtab, 10);
//    RecordDesc rd;
//    rd.addTable ("tab1", "td");            // with description name
//    rd.addField ("tab2", TpTable);         // without description name
//    TableRecord rec (rd, RecordInterface::Variable);
//    // Both define's are possible.
//    // The first one because the table description name matches.
//    // The second one because that field has no table description name,
//    // thus every table description matches.
//    rec.defineTable (rec.fieldNumber("tab1"), tab1);
//    rec.defineTable (rec.fieldNumber("tab2"), tab1);
//    Table t1 = rec.asTable ("tab1");
//    AlwaysAssertExit (t1.nrow() == 10  &&  t1.tableDesc().ncolumn() == 2);
//    Table t2 = rec.asTable ("tab2");
//    AlwaysAssertExit (t2.nrow() == 10  &&  t2.tableDesc().ncolumn() == 2);
//    AipsIO aos ("file.name", ByteIO::New);
//    aos << rec;
//  }
//    // Note that he above is put in a separate scope to be sure that
//    // all objects are deleted and tables are written.
//  {
//    TableRecord rec;
//    AipsIO aos ("file.name");
//    aos >> rec;
//    // At this point the record is read back, but the tables are not opened.
//    // The next statement accesses the table resulting in its open.
//    Table t1 = rec.asTable ("tab1");
//    // The following statement closes it again.
//    rec.closeTable ("tab1");
// </srcblock>
// </example>
//
// <motivation>
// In principle the class Record could also support data type Table.
// However, this would have had the big disadvantage that all the
// Table code would have be linked in when only a simple Record is needed.
// It was decided that for that reason it was better to support tables
// in a separate class.
// </motivation>
//
// <todo asof="1995/08/22">
//   <li> A record reference class, which contains some fields from another
//        record, would likely be useful. This would be analagous to a
//        subarray sliced from an existing array.
// </todo>


class TableRecord : public RecordInterface
{
friend class TableRecordRep;

public:
    // Create a record with no fields.
    // The record has a variable structure.
    TableRecord();

    // Create a record with no fields.
    // The type determines if the record has a fixed or variable structure.
    // The callback function is called when a field is added to the Record.
    // That function can check the name and of data type of the new field
    // (for instance, the Table system uses it to ensure that table columns
    // and keywords have different names).
    explicit TableRecord (RecordType type,
			  CheckFieldFunction* = 0,
			  const void* checkArgument = 0);

    // Create a record with the given description. If it is not possible to 
    // create all fields (for example, if a field with an unsupported data
    // type is requested), an exception is thrown.
    // The type determines if the record has a fixed or variable structure.
    // All fields are checked by the field checking function (if defined)
    // (for instance, the Table system uses it to ensure that table columns
    // and keywords have different names).
    explicit TableRecord (const RecordDesc& description,
			  RecordType type = Fixed,
			  CheckFieldFunction* = 0, 
			  const void* checkArgument = 0);

    // Create a copy of other using copy semantics.
    TableRecord (const TableRecord& other);

    // Create a TableRecord from another type of record.
    // It uses copy-on-write semantics if possible (i.e. if
    // <src>other</src> is a TableRecord), otherwise each field is copied.
    // Subrecords are also copied and converted to TableRecords if needed.
    TableRecord (const RecordInterface& other);

    // Copy the data in the other record to this record.
    // It can operate in 2 ways depending on the TableRecord structure flag.
    // <ul>
    // <li> For variable structured records the existing fields are
    //      thrown away and replaced by the new fields.
    //      This means that RecordFieldPtr's using this record get invalidated.
    //      Because copy-on-write semantics are used, this kind of
    //      assignment is a very efficient operation.
    // <li> For fixed structured records the existing values are replaced
    //      by the new values. This means that RecordFieldPtr's using this
    //      record remain valid.
    //      The structure of the other record has to conform this record
    //      or this record has to be empty, otherwise an exception is thrown.
    //      This assignment is less efficient, because it has to check the
    //      conformance and because each value has to be copied.
    // </ul>
    // <note role=warning>
    // Attributes like fixed structure flag and check function will not
    // be copied.
    // </note>
    TableRecord& operator= (const TableRecord& other);
    
    // Release resources associated with this object.
    ~TableRecord();

    // Make a copy of this object.
    virtual RecordInterface* clone() const;

    // Assign that RecordInterface object to this one.
    // If <src>that</src> is a TableRecord, copy-on-write is used.
    // Otherwise each individual field is copied.
    virtual void assign (const RecordInterface& that);

    // Get the comment for this field.
    virtual const String& comment (const RecordFieldId&) const;

    // Set the comment for this field.
    virtual void setComment (const RecordFieldId&, const String& comment);

    // Describes the current structure of this TableRecord.
    const RecordDesc& description() const;

    // Change the structure of this TableRecord to contain the fields in
    // newDescription. After calling restructure, <src>description() ==
    // newDescription</src>. Any existing RecordFieldPtr objects are
    // invalidated (their <src>isAttached()</src> members return False) after
    // this call.
    // <br>When the new description contains subrecords, those subrecords
    // will be restructured if <src>recursive=True</src> is given.
    // Otherwise the subrecord is a variable empty record.
    // Subrecords will be variable if their description is empty (i.e. does
    // not contain any field), otherwise they are fixed.
    // <br>Restructuring is not possible and an exception is thrown
    // if the Record has a fixed structure.
    virtual void restructure (const RecordDesc& newDescription,
			      Bool recursive=True);

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
    Bool conform (const TableRecord& other) const;

    // How many fields does this structure have? A convenient synonym for
    // <src>description().nfields()</src>.
    virtual uInt nfields() const;

    // Get the field number from the field name.
    // -1 is returned if the field name is unknown.
    virtual Int fieldNumber (const String& fieldName) const;

    // Get the data type of this field.
    virtual DataType type (Int whichField) const;

    // Remove a field from the record.
    // <note role=caution>
    // Removing a field means that the field number of the fields following
    // it will be decremented. Only the RecordFieldPtr's
    // pointing to the removed field will be invalidated.
    // </note>
    void removeField (const RecordFieldId&);

    // Rename the given field.
    void renameField (const String& newName, const RecordFieldId&);

    // Define a value for the given field.
    // When the field is unknown, it will be added to the record.
    // The second version is meant for any type of record (e.g. Record,
    // TableRecord, GlishRecord). It is converted to a TableRecord using the
    // TableRecord constructor taking a RecordInterface object.
    // <group>
    void defineRecord (const RecordFieldId&, const TableRecord& value,
		       RecordType type = Variable);
    virtual void defineRecord (const RecordFieldId&,
			       const RecordInterface& value,
			       RecordType = Variable);
    void defineTable  (const RecordFieldId&, const Table& value,
		       RecordType type = Variable);
    // </group>

    // Get the subrecord or table from the given field.
    // <note>
    // The non-const version has a different name to prevent that the
    // copy-on-write mechanism makes a copy when not necessary.
    // </note>
    // <group>
    const TableRecord& subRecord (const RecordFieldId&) const;
    TableRecord& rwSubRecord (const RecordFieldId&);
    virtual const RecordInterface& asRecord (const RecordFieldId&) const;
    virtual RecordInterface& asrwRecord (const RecordFieldId&);
    // </group>

    // Get the table from the given field.
    // By default the read/write option and lock options are inherited
    // from the parent table.
    // If openWritable=True, the table is still opened as readonly if the file
    // permissions do not permit write access.
    // <group>
    Table asTable (const RecordFieldId&) const;
    Table asTable (const RecordFieldId&, const TableLock& lockOptions) const;
    // </group>

    // Get the attributes of a table field.
    const TableAttr& tableAttributes (const RecordFieldId&) const;

    // Merge a field from another record into this record.
    // The DuplicatesFlag (as described in
    // <linkto class=RecordInterface>RecordInterface</linkto>) determines
    // what will be done in case the field name already exists.
    void mergeField (const TableRecord& other, const RecordFieldId&,
		     DuplicatesFlag = ThrowOnDuplicates);

    // Merge all fields from the other record into this record.
    // The DuplicatesFlag (as described in
    // <linkto class=RecordInterface>RecordInterface</linkto>) determines
    // what will be done in case a field name already exists.
    // An exception will be thrown if other is the same as this
    // (i.e. if merging the record itself).
    void merge (const TableRecord& other, DuplicatesFlag = ThrowOnDuplicates);
    
    // Close the table in the given field.
    // When accessed again, it will be opened automatically.
    // This can be useful to save memory usage.
    void closeTable (const RecordFieldId&) const;

    // Close all open tables.
    // When accessed again, it will be opened automatically.
    // This can be useful to save memory usage.
    void closeTables() const;

    // Flush all open subtables.
    void flushTables (Bool fsync=False) const;

    // Rename the subtables with a path containing the old parent table name.
    void renameTables (const String& newParentName,
		       const String& oldParentName);

    // Are subtables used in other processes.
    Bool areTablesMultiUsed() const;

    // Write the TableRecord to an output stream.
    friend AipsIO& operator<< (AipsIO& os, const TableRecord& rec);

    // Read the TableRecord from an input stream.
    friend AipsIO& operator>> (AipsIO& os, TableRecord& rec);

    // Put the data of a record.
    // This is used to write a subrecord, whose description has
    // not been written.
    void putRecord (AipsIO& os, const TableAttr&) const;

    // Read a record.
    // This is used to read a subrecord, whose description has
    // not been read.
    void getRecord (AipsIO& os, const TableAttr&);

    // Put the data of a record.
    // This is used to write a subrecord, whose description has
    // already been written.
    void putData (AipsIO& os, const TableAttr&) const;

    // Read the data of a record.
    // This is used to read a subrecord, whose description has
    // already been read.
    void getData (AipsIO& os, uInt version, const TableAttr&);

    // Print the contents of the record.
    // Only the first <src>maxNrValues</src> of an array will be printed.
    // A value < 0 means the entire array.
    virtual void print (std::ostream&,
			Int maxNrValues = 25,
			const String& indent="") const;

    // Reopen possible tables in keywords as read/write.
    // Tables are not reopened if they are not writable.
    void reopenRW();

    // Recursively set the attributes of subtables to the ones in the other
    // record for matching subtable field names. Otherwise set it to defaultAttr.
    // The name attribute is not changed.
    // It is primarily a helper function for PlainTable::syncTable
    // and ColumnSet::syncColumns.
    // <br>However, it can also be used to achieve that all subtables of a
    // read/write table are opened as readonly. E.g.:
    // <srcblock>
    //   TableAttr newAttr(String(), False, mainTable.lockOptions());
    //   mainTable.keywordSet().setTableAttr (TableRecord(), newAttr);
    // </srcblock>
    void setTableAttr (const TableRecord& other, const TableAttr& defaultAttr);

    // Make a unique record representation
    // (to do copy-on-write in RecordFieldPtr).
    virtual void makeUnique();


protected:
    // Used by the RecordField classes to attach in a type-safe way to the
    // correct field.
    // <group>
    virtual void* get_pointer (Int whichField, DataType type) const;
    virtual void* get_pointer (Int whichField, DataType type,
			       const String& recordType) const;
    // </group>

    // Return a const reference to the underlying TableRecordRep.
    const TableRecordRep& ref() const;

    // Return a non-const reference to the underlying TableRecordRep.
    // When needed, the TableRecordRep will be copied and all RecordField
    // objects will be notified.
    TableRecordRep& rwRef();

    // Add a field to the record.
    virtual void addDataField (const String& name, DataType type,
			       const IPosition& shape, Bool fixedShape,
			       const void* value);

    // Define a value in the given field.
    virtual void defineDataField (Int whichField, DataType type,
				  const void* value);

private:
    // Get the description of this record.
    virtual RecordDesc getDescription() const;

    // Create TableRecord as a subrecord.
    // When the description is empty, the record has a variable structure.
    // Otherwise it is fixed.
    // <group>
    TableRecord (TableRecordRep* parent, const RecordDesc& description);
    TableRecord (TableRecordRep* parent, RecordType type);
    // </group>

    // Set the recordtype of this record and all its subrecords (recursively).
    void setRecordType (RecordType type);

    // The TableRecord representation.
    COWPtr<TableRecordRep> rep_p;
    // The parent TableRecord.
    TableRecordRep* parent_p;
};



inline const TableRecordRep& TableRecord::ref() const
{
    return rep_p.ref();
}
inline const RecordDesc& TableRecord::description() const
{
    return ref().description();
}

inline Bool TableRecord::conform (const TableRecord& other) const
{
    return ref().conform (other.ref());
}

inline void TableRecord::putData (AipsIO& os,
				  const TableAttr& parentAttr) const
{
    ref().putData (os, parentAttr);
}

inline void TableRecord::getData (AipsIO& os, uInt version,
				  const TableAttr& parentAttr)
{
    rwRef().getData (os, version, parentAttr);
}

inline void TableRecord::reopenRW()
{
    rwRef().reopenRW();
}

inline void TableRecord::closeTables() const
{
    ref().closeTables();
}

inline void TableRecord::flushTables (Bool fsync) const
{
    ref().flushTables (fsync);
}

inline void TableRecord::renameTables (const String& newParentName,
				       const String& oldParentName)
{
    rwRef().renameTables (newParentName, oldParentName);
}

inline Bool TableRecord::areTablesMultiUsed() const
{
    return ref().areTablesMultiUsed();
}



} //# NAMESPACE CASACORE - END

#endif
